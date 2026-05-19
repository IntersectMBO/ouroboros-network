cbor = Dissector.get("cbor")

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
	[0x0008] = "KeepAlive Initiator",
	[0x800a] = "Peer Sharing Responder",
	[0x000a] = "Peer Sharing Initiator"
}
local on_conversation = ProtoField.uint16("ouroboros.conv", "Conversation", base.HEX, conv_ids, nil, "Conversation ids")

local handshake_msg_codes = {
	[0] = "MsgProposeVersions",
	[1] = "MsgAcceptVersion",
	[2] = "MsgRefuse",
	[3] = "MsgQueryReply"
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

local peersharing_msg_codes = {
	[0] = "MsgShareRequest",
	[1] = "MsgSharePeers",
	[2] = "MsgDone"
}

local on_peersharing_msg = ProtoField.uint8("ouroboros.peersharingmsg", "PeerSharing Message", base.DEC, peersharing_msg_codes, nil, "PeerSharing Message Types")

local on_cbor_payload = ProtoField.bytes("ouroboros.cbor_payload",
                                         "CBOR payload (raw)", base.NONE)

ouroboros.fields = {
	on_transmission_time,
	on_conversation,
	on_length,
	on_handshake_msg,
	on_chainsync_msg,
	on_blockfetch_msg,
	on_txsubmission_msg,
	on_keepalive_msg,
	on_peersharing_msg,
	on_cbor_payload
}


function ouroboros.dissector(tvbuf, pktinfo, root)

	local pktlen = tvbuf:len()
	local offset = 0
	local total = 0

	while offset < pktlen do
		result = dissectOuroboros(tvbuf, pktinfo, root, offset)
		if result > 0 then
			-- We parsed an Ouroboros message
			offset = offset + result
			total = total + result
		elseif result < 0 then
			-- we need more bytes for current frame
			pktinfo.desegment_offset = offset
			pktinfo.desegment_len = -result
			return -result
		else
			-- some error
			break
		end
	end

	return total
end

dissectOuroboros = function (tvbuf, pktinfo, root, offset)

	local pktlen = tvbuf:len()
	local msglen = pktlen - offset

	if msglen < ON_HDR_LEN then
		-- Not enough data to read the header, we need at least one more segmet.
		local need = ON_HDR_LEN - msglen
		return -need
	end

	local ts_buf   = tvbuf:range(offset, 4)     -- 4-byte timestamp
	local conv_buf = tvbuf:range(offset + 4, 2) -- 2-byte conv id
	local len_buf  = tvbuf:range(offset + 6, 2) -- 2-byte CBOR length

	local ts   = ts_buf:uint()
	local convId = conv_buf:uint()
	local len  = len_buf:uint()

	local total_len = ON_HDR_LEN + len
	if msglen < total_len then
		-- Not enough data for the whole payload, ask for the rest
		local need = total_len - msglen
		return -need
	end

	-- We have a complete mux frame.
	pktinfo.cols.protocol = ouroboros.name

	local subtree = root:add(ouroboros, tvbuf(offset, total_len),
				string.format("Ouroboros (%u bytes)", total_len))
	subtree:add(on_transmission_time, ts_buf)
	subtree:add(on_conversation, conv_buf)
	subtree:add(on_length, len_buf)

	if on_cbor_payload then
		subtree:add(on_cbor_payload, tvbuf(offset + ON_HDR_LEN, len))
	end

	if len > 0 then
		local miniProt = nil
		local miniVal = convId & 0x7fff

		if     miniVal == 0  then miniProt = on_handshake_msg
		elseif miniVal == 2  then miniProt = on_chainsync_msg
		elseif miniVal == 3  then miniProt = on_blockfetch_msg
		elseif miniVal == 4  then miniProt = on_txsubmission_msg
		elseif miniVal == 8  then miniProt = on_keepalive_msg
		elseif miniVal == 10 then miniProt = on_peersharing_msg
		end

		if miniProt and len > 1 then
			subtree:add(miniProt, tvbuf(offset + ON_HDR_LEN + 1, 1))
		end


		if not cbor then
			subtree:add_expert_info(PI_MALFORMED, PI_ERROR, "CBOR dissector not found")
		else
			local cbor_tvb = tvbuf:range(offset + ON_HDR_LEN, len):tvb()
			local success, err = pcall(function()
				cbor:call(cbor_tvb, pktinfo, subtree)
			end)

			if not success then
				subtree:add_expert_info(PI_MALFORMED, PI_WARN,
					"Possible partial CBOR message")
			end
		end
	end

	return total_len

end

local tcp_port = DissectorTable.get("tcp.port")
tcp_port:add(6061, ouroboros)

-- vim: noexpandtab
