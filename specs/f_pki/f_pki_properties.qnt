module f_pki_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import f_pki_types.*   from "./f_pki_types"
    import f_pki.*  from "./f_pki"

    val Parties = Set(1,2,3)
    val sids = Set(4,5)
    val all_certs = Set(1001,1002,1003)
    val all_vks = Set(201,202,203,204)

    action step : bool = any {
        pki_step,  
        env_pki_send_all_reqs,
        env_consume_responses,
    }    

    action init = all {
        pki_init(Parties, all_certs, sids),
    }    

    action env_pki_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()

        val cfg = {party: from, sid: sid, nonce: pki_s.nonce}
        any {
            env_pki_register_reqs(cfg),
            env_pki_retrieve_reqs(cfg),
            env_pki_verify_reqs(cfg),
            env_pki_sim_reqs,
        }
    }

    action env_pki_sim_reqs : bool = all {
        pki_s.input.size() == 0,
        pki_s' = {...pki_s, 
            input: Set(SIMControl),
            nonce: pki_s.nonce + 1

        }
    }    

    action env_pki_register_reqs(cfg: MsgConfig) : bool = all {
        pki_s.input.size() == 0,
        
        nondet vk = all_vks.oneOf()
        
        pki_s' = {...pki_s, 
            input: Set(RegisterRequest({cfg: cfg, vk: vk})),
            nonce: pki_s.nonce + 1
        }
    }

    action env_pki_retrieve_reqs(cfg: MsgConfig) : bool = all {
        pki_s.input.size() == 0,
        
        nondet Q = Parties.oneOf()
        
        pki_s' = {...pki_s, 
            input: Set(RetrieveRequest({cfg: cfg, Q: Q})),
            nonce: pki_s.nonce + 1
        }
    }    

    action env_pki_verify_reqs(cfg: MsgConfig) : bool = all {
        pki_s.input.size() == 0,
        
        nondet Q = Parties.oneOf()
        nondet vk = all_vks.oneOf()
        nondet cert = all_certs.oneOf()
        
        pki_s' = {...pki_s,  // Q: Party, vk: VK, cert: Certificate
            input: Set(CertVerifyRequest({cfg: cfg, Q: Q, vk: vk, cert: cert})),
            nonce: pki_s.nonce + 1
        }
    }        


    pure def env_consume_response(pki: PKI_System) : Set[PKI_Transition] = {
         pki.input.pki_get_all_response_messages().map(msg_log =>
         {
            post_state : pki.state,
            effects : Set(
                PKI_LogMessage(msg_log),
                PKI_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(pki_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        pki_s' = pki_process_transitions(pki_s, transition),
    }



    // one entry per party in Reg and Ver lists
    val OEPP = Parties.forall(p => sids.forall({sid =>
        and { 
            pki_s.state.get(sid).Ver.filter(x => x.party == p).size() <= 1,
            pki_s.state.get(sid).Reg.filter(x => x.party == p).size() <= 1
        }
    }))
    
    // RetrieveSucceedsForRegistered
    val RSFR = pki_s.log.forall(entry1 => {
        match entry1.msg {
        | RegisterResponse(reg_resp) =>
            pki_s.log.forall(entry2 => {
                match entry2.msg {
                    | RetrieveResponse(ret_resp) => {
                        and {
                            reg_resp.cfg.sid == ret_resp.cfg.sid,
                            reg_resp.cfg.party == ret_resp.Q,
                            entry1.idx < entry2.idx
                        } implies {
                            ret_resp.vk == Some(
                                pki_s.state.get(reg_resp.cfg.sid).Reg.find(x => x.party == reg_resp.cfg.party).unwrap().vk
                            )
                        }
                    }
                    | _ => true
                }
            })
        | _ => true
        }
    })

    // RetrieveFailsForUnregistered
    val RFFU = pki_s.log.forall(entry1 => {
        match entry1.msg {
        | RetrieveResponse(ret_resp) => {
            
            val reg_responses = pki_s.log.filter(entry2 => {
                match entry2.msg {
                    | RegisterResponse(reg_resp) => {
                        and {
                            reg_resp.cfg.sid == ret_resp.cfg.sid,
                            reg_resp.cfg.party == ret_resp.Q,
                            entry2.idx < entry1.idx
                        }
                    }
                    | _ => false
                }
            }).size()

            if (reg_responses == 0) {
                ret_resp.vk == None
            } else {
                ret_resp.vk != None
            }

        }
        | _ => true
        }
    })    

    val VFFU = pki_s.log.forall(entry1 => {
        match entry1.msg {
        | CertVerifyResponse(ver_resp) => {
            
            val reg_responses = pki_s.log.filter(entry2 => {
                match entry2.msg {
                    | RegisterResponse(reg_resp) => {
                        and {
                            reg_resp.cfg.sid == ver_resp.cfg.sid,
                            reg_resp.cfg.party == ver_resp.Q,
                            entry2.idx < entry1.idx,
                            reg_resp.cert == Some(ver_resp.cert),
                            reg_resp.vk == ver_resp.vk
                        }
                    }
                    | _ => false
                }
            }).size()

            if (reg_responses == 0) {
                ver_resp.result == false
            } else{
                ver_resp.result == true
            }

        }
        | _ => true
        }
    })    

}