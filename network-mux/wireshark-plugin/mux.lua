-- WireShark mux disector
--
-- It is based on the
-- ./ouroboros-network/wireshark-plugin/ouroboros_network.lua
-- But it only dissects MuxSDU's, not protocol messages carries inside of it.
--
-- To install the pluging, create a symbolic link:
-- ```bash
-- ls -s $(pwd)/network-mux/demo/mux-leios-demo.lua $HOME/.config/wireshark/plugins/mux-leios-demo.lua
-- ```

local default_settings = {
  -- the port used by `./network-mux/demo/mux-leios-demo.sh`
  port = 9001,
  -- a range of mini-protocol numbers is used to check if we parsed a valid
  -- MuxSDU header.  This is important when TCP segments are lost, and we need
  -- to skip bytes until we find a begining of a new MuxSDU.
  min_mini_protocol_num = 2,
  max_mini_protocol_num = 3,
}

local MUX_HDR_LEN = 8

local mux = Proto("Mux", "LeiosDemo")

local mux_transmission_time = ProtoField.new ("mux.time", "mux.time", ftypes.UINT32)
local mux_mini_protocol     = ProtoField.uint16("mux.mini-protocol-num", "min-protocol-num", base.HEX)
local mux_length            = ProtoField.new ("mux.length", "mux.length", ftypes.UINT16)
local mux_payload           = ProtoField.bytes("mux.payload", "mux.payload", base.NONE)

mux.fields = {
  mux_transmission_time,
  mux_mini_protocol,
  mux_length,
  mux_payload
}


function mux.dissector(tvbuf, pktinfo, root)

  local pktlen = tvbuf:len()
  local offset = 0
  local total  = 0

  -- Parse possibly multiple MuxSDU's in the TCP segment.
  while offset < pktlen do
    local result = dissectMux(tvbuf, pktinfo, offset, false)
    if result.mux_length > 0 then
      -- A compate MuxSDU was parsed.
      pktinfo.cols.protocol = mux.name
      local subtree = root:add(mux, tvbuf(offset, result.mux_length),
                               string.format("mux (%u bytes)", result.mux_length))
      subtree:add(mux_transmission_time, result.transmission_time_buf)
      subtree:add(mux_mini_protocol, result.mini_protocol_num_buf)
      subtree:add(mux_length, result.len_buf)
      subtree:add(mux_payload, result.payload_buf)

      -- advance offset and total for the while loop
      offset = offset + result.mux_length
      total  = total + result.mux_length

    elseif result.mux_length < 0 then
      -- We need more bytes to parse current MuxSDU.
      pktinfo.desegment_offset = offset
      pktinfo.desegment_len    = -result.mux_length
      return

    else
      -- some error
      break
    end
  end

  return total
end

-- A generic helper function which disects a single message.  It returns
-- a record with
--
-- ```
-- { mux_length
-- , transmission_time_buf
-- , mini_protocol_num_buf
-- , len_buf
-- , payload_buf
-- }
-- ```
-- `mux_length` is
-- * positive length of used input bytes on success
-- * negative number of needed bytes if more data is required
--
dissectMux = function (tvbuf, pktinfo, offset, skipping_bytes)
  local pktlen = tvbuf:len()
  local msglen = pktlen - offset

  if msglen < MUX_HDR_LEN then
    if skipping_bytes then
      -- we tried to resync by skipping bytes, but we didn't get it right.
      -- We'll skip this packet.  There's still a chance that we just missed
      -- a few bytes...
      return { mux_length = 0 }
    else
      -- Not enough data to read the header, we need at least one more segmet.
      local need = MUX_HDR_LEN - msglen
      return { mux_length = -need }
    end
  end

  local transmission_time_buf = tvbuf:range(offset, 4)     -- 4-byte timestamp
  local mini_protocol_num_buf = tvbuf:range(offset + 4, 2) -- 2-byte conv id
  local len_buf               = tvbuf:range(offset + 6, 2) -- 2-byte SDU payload length
  local len                   = len_buf:uint()

  -- A simple sanity check on mini-protocol numbers.
  --
  -- When TCP segments are lost, we need to skip until we find the begining of
  -- next MuxSDU.
  local mini_protocol_num = mini_protocol_num_buf:uint() & 0x7fff
  if   mini_protocol_num < default_settings.min_mini_protocol_num
    or mini_protocol_num > default_settings.max_mini_protocol_num then
    return dissectMux(tvbuf, pktinfo, offset + 1, true)
  end

  local mux_length = MUX_HDR_LEN + len
  if msglen < mux_length then
    -- not enough data for the whole payload, ask for the rest
    local need = mux_length - msglen
    return { mux_length = -need }
  end
  local payload_buf = tvbuf(offset + 8, len)

  return { mux_length            = mux_length
         , transmission_time_buf = transmission_time_buf
         , mini_protocol_num_buf = mini_protocol_num_buf
         , len_buf               = len_buf
         , payload_buf           = payload_buf
         }
end

local tcp_port = DissectorTable.get("tcp.port")
tcp_port:add(default_settings.port, mux)


-- Export the `mux` dissector, `dissectMux` helper function, and MuxSDU header
-- length constant.
return { mux = mux
       , dissectMux = dissectMux
       , muxHdrLen = MUX_HDR_LEN
       }
