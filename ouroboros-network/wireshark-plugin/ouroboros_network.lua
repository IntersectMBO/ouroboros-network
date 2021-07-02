
local default_settings =
{
	port = 6061 -- XXX Should be somethig else
}

local ouroboros = Proto("Ouroboros", "Ouroboros")

local on_transmission_time = ProtoField.new ("Transmission Time", "ouroboros.ts", ftypes.UINT32)
local on_length = ProtoField.new ("Length", "ouroboros.length", ftypes.UINT16)

local conv_ids = {
	[0x0000] = "Handshake Initiator",
	[0x8000] = "Handshake Responder",
	[0x0001] = "DeltaQueue Initiator",
	[0x8001] = "DeltaQueue Responder",
	[0x0002] = "ChainSync Initiator",
	[0x8002] = "ChainSync Responder",
	[0x0003] = "BlockFetch Initiator",
	[0x8003] = "BlockFetch Responder",
	[0x8004] = "TxSubmission Responder",
	[0x0004] = "TxSubmission Initiator",
	[0x8008] = "KeepAlive Responder",
	[0x0008] = "KeepAlive Initiator"
}
local on_conversation = ProtoField.uint16("ouroboros.conv", "Conversation", base.HEX, conv_ids, nil, "Conversation ids")

local handshake_msg_codes = {
	[0] = "MsgProposeVersions",
	[1] = "MsgAcceptVersion",
	[2] = "MsgRefuse"
}
local on_handshake_msg = ProtoField.uint8("ouroboros.handshakemsg", "Handshake Message", base.DEC, handshake_msg_codes, nil, "Handshake Message Types")

local chainsync_msg_codes = {
	[0] = "MsgRequestNext",
	[1] = "MsgAwaitReply",
	[2] = "MsgRollForward",
	[3] = "MsgRollBackward",
	[4] = "MsgFindIntersect",
	[5] = "MsgIntersectFound",
	[6] = "MsgIntersectNotFound",
	[7] = "MsgDone"
}

local on_chainsync_msg = ProtoField.uint8("ouroboros.chainmsg", "ChainSync Message", base.DEC, chainsync_msg_codes, nil, "ChainSync Message Types")

local blockfetch_msg_codes = {
	[0] = "MsgRequestRange",
	[1] = "MsgClientDone",
	[2] = "MsgStartBatch",
	[3] = "MsgNoBlocks",
	[4] = "MsgBlock",
	[5] = "MsgBatchDone"
}

local on_blockfetch_msg = ProtoField.uint8("ouroboros.blockmsg", "BlockFetch Message", base.DEC, blockfetch_msg_codes, nil, "BlockFetch Message Types")

local ON_HDR_LEN = 8

local txsubmission_msg_codes = {
	[0] = "MsgRequestTxIds",
	[1] = "MsgReplyTxIds",
	[2] = "MsgRequestTxs",
	[3] = "MsgReplyTxs",
	[4] = "MsgDone",
	[6] = "MsgHello"
}

local on_txsubmission_msg = ProtoField.uint8("ouroboros.txsubmsg", "TxSubmission Message", base.DEC, txsubmission_msg_codes, nil, "TxSubmission Message Types")

local ON_HDR_LEN = 8

local keepalive_msg_codes = {
	[0] = "MsgKeepAlive",
	[1] = "MsgKeepAliveResponse",
	[2] = "MsgDone"
}

local on_keepalive_msg = ProtoField.uint8("ouroboros.keepalivemsg", "KeepAlive Message", base.DEC, keepalive_msg_codes, nil, "KeepAlive Message Types")


ouroboros.fields = {
	on_transmission_time,
	on_conversation,
	on_length,
	on_handshake_msg,
	on_chainsync_msg,
	on_blockfetch_msg,
	on_txsubmission_msg,
	on_keepalive_msg
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

	if (convId >= 0x8000) then convId = convId - 0x8000
	end

	subtree:add(on_transmission_time, tvbuf:range(offset, 4))
	subtree:add(on_conversation, conv)
	subtree:add(on_length, length)

	-- XXX Without completly parsing the CBOR payload we can't be sure that this is
	-- the correct message type since a CBOR message may be split into
	-- multiple MUX segements.
	if     convId == 0 then subtree:add(on_handshake_msg, tvbuf:range(offset + 9, 1))
	elseif convId == 2 then subtree:add(on_chainsync_msg, tvbuf:range(offset + 9, 1))
	elseif convId == 3 then subtree:add(on_blockfetch_msg, tvbuf:range(offset + 9, 1))
	elseif convId == 4 then subtree:add(on_txsubmission_msg, tvbuf:range(offset + 9, 1))
	elseif convId == 8 then subtree:add(on_keepalive_msg, tvbuf:range(offset + 9, 1))
	end

	return ON_HDR_LEN + length
end

local tcp_port = DissectorTable.get("tcp.port")
tcp_port:add(6061, ouroboros)

-- vim: noexpandtab
