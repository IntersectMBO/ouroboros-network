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

-- the port used by `./network-mux/demo/mux-leios-demo.sh`
local default_settings = {
  port = 9001
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
    local result = dissectMux(tvbuf, pktinfo, root, offset)
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
dissectMux = function (tvbuf, pktinfo, root, offset)
  local pktlen = tvbuf:len()
  local msglen = pktlen - offset

  if msglen < MUX_HDR_LEN then
    -- Not enough data to read the header, we need at least one more segmet.
    local need = MUX_HDR_LEN - msglen
    return { mux_length = -need }
  end

  local transmission_time_buf = tvbuf:range(offset, 4)     -- 4-byte timestamp
  local mini_protocol_num_buf = tvbuf:range(offset + 4, 2) -- 2-byte conv id
  local len_buf               = tvbuf:range(offset + 6, 2) -- 2-byte SDU payload length
  local len                   = len_buf:uint()

  -- a simply sanity check on mini-protocol number, currently we don't have
  -- mini-protocols numbers above 20
  --
  -- TODO: this was added to ignore TCP frames after a segment was lost, and
  -- parsing got out of sync.  It only get's up to sync again, when a new TCP
  -- frame starts with a new MuxSDU.
  local mini_protocol_num = mini_protocol_num_buf:uint() & 0x7fff
  if mini_protocol_num > 20 then
    return { mux_length = 0 }
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
