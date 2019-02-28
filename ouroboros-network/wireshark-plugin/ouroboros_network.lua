
local default_settings =
{
	port = 6061 -- XXX Should be somethig else
}

local ouroboros = Proto("Ouroboros", "Ouroboros")

local on_transmission_time = ProtoField.new ("Transmission Time", "ouroboros.ts", ftypes.UINT32)
local on_length = ProtoField.new ("Length", "ouroboros.length", ftypes.UINT16)

local conv_ids = {
	[0x0000] = "MuxControl Initiator",
	[0x8000] = "MuxControl Responder",
	[0x0001] = "DeltaQueue Initiator",
	[0x8001] = "DeltaQueue Responder",
	[0x0002] = "ChainSync Initiator",
	[0x8002] = "ChainSync Responder"
}
local on_conversation = ProtoField.uint16("ouroboros.conv", "Conversation", base.HEX, conv_ids, nil, "Conversation ids")

local chainsync_msg_codes = {
	[0] = "MsgRequestNext",
	[1] = "MsgAwaitReply",
	[2] = "MsgRollForward",
	[3] = "MsgRollBackward",
	[4] = "MsgFindIntersect",
	[5] = "MsgIntersectImproved",
	[6] = "MsgIntersectUnchanged",
	[7] = "MsgDone"
}

local on_chainsync_msg = ProtoField.uint8("ouroboros.msg", "ChainSync Message", base.DEC, chainsync_msg_codes, nil, "ChainSync Message Types")

local ON_HDR_LEN = 8

ouroboros.fields = {
	on_transmission_time,
	on_conversation,
	on_length,
	on_chainsync_msg
}

function ouroboros.dissector(tvbuf, pktinfo, root)

	local pktlen = tvbuf:len()
	local offset = 0

	while offset < pktlen do
		result = dissectOuroboros(tvbuf, pktinfo, root, offset)
		if result > 0 then
			-- We parsed an Ouroboros message
			offset = offset + result
		else
			-- we need more bytes
			pktinfo.desegment_offset = offset
			result = -result
			pktinfo.desegment_len = result

			return pktlen
		end
	end
end

dissectOuroboros = function (tvbuf, pktinfo, root, offset)

	local msglen = tvbuf:len() - offset
	if msglen < ON_HDR_LEN then
		-- Not enough data to read the header, we need at least one more segmet.
		return -DESEGMENT_ONE_MORE_SEGMENT
	end

	local on_length_buf = tvbuf:range(offset + 6, 2)
	local length = on_length_buf:uint()

	if msglen < ON_HDR_LEN + length then
		-- more bytes needed to read the whole message
		return -(length - msglen + ON_HDR_LEN)
	end
	
	pktinfo.cols.protocol = ouroboros.name

 	local subtree = root:add(ouroboros, tvbuf(), "Ouroboros")
	local conv = tvbuf:range(offset + 4, 2)
	local convId = conv:uint()

	subtree:add(on_transmission_time, tvbuf:range(offset, 4))
	subtree:add(on_conversation, conv)
	subtree:add(on_length, length)

	if convId == 2 or convId == 0x8002 then
		-- XXX Without completly parsing the CBOR payload we can't be sure that this is
		-- the correct message type since a CBOR message may be split into
		-- multiple MUX segements.
		subtree:add(on_chainsync_msg, tvbuf:range(offset + 9, 1))
	end

	return ON_HDR_LEN + length
end

local tcp_port = DissectorTable.get("tcp.port")
tcp_port:add(6061, ouroboros)
