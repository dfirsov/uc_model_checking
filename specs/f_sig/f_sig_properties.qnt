module f_sig_properties {
    
    import basicSpells.* from "../../spells/basicSpells"
    import f_sig_types.* from "./f_sig_types"
    import f_sig.* from "./f_sig"
    import f_sig_env.* from "./f_sig_env"
    import common.* from "../common"    


    // FUST FAILS   
    val GenLiveness  = sids.forall(sid => f_sig_s.state.get(sid).VK.size() <= Signers.size() - 1) 
    val SignLiveness = sids.forall(sid => f_sig_s.state.get(sid).Ver.size() <= 5) 
    // If Ver containts some vk entry  then VK table must  contain respective entry
    val AdvCanVerify = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        sid_state.Ver.forall(
            entry => sid_state.VK.exists(p => p.vk == entry.vk)
        )
    })

    // MUST HOLD
    // Property: party has at most one verification key
    val OneVKperUser = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        Parties.forall(p => {
            sid_state.VK.filter(x => x.party == p).size() <= 1
        })
    })
    // Property: verification keys are unique 
    val VKUnique = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        val allVKs = sid_state.VK.map(p => p.vk)
        allVKs.forall(p => {
            sid_state.VK.filter(x => x.vk == p).size() == 1
        })
    })
    // Property: verification results are consistent (no contradictory entries)
      val VerConsistent = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        sid_state.Ver.forall(entry => {
            not ({...entry, b : not(entry.b)}.in(sid_state.Ver))
        })
      })

     
    // Completeness: If sign response has signature then later it verifies successfully
    val Completeness = f_sig_s.log.forall(log1 => {
        match log1.msg {
            | SignResponse(p) => {
                f_sig_s.log.forall(log2 => {
                    match log2.msg {
                        | VerifyResponse(q) => {
                            and {
                                log1.idx < log2.idx, // sign response comes before verify response
                                p.cfg.sid == q.cfg.sid,
                               p.sig != None,
                                p.m == q.m,
                                q.vk == f_sig_s.state.get(q.cfg.sid).VK.find(vk_entry => vk_entry.party == p.cfg.party).unwrap().vk, 
                            } implies ({
                                and {
                                    p.sig.unwrap() == q.sig,
                                    //true
                                } implies {
                                    q.b == true
                                    //false
                                }
                            })
                        }
                        | _ => true
                    }
                })
            }
            |_ => true
        }
    })

    // Soundness: If message verifies with the registered vk then earlier it was signed
    val Soundness = f_sig_s.log.forall(log1 => {
        match log1.msg {
            | VerifyResponse(q) => {
                val sid_state = f_sig_s.state.get(q.cfg.sid)
                (sid_state.VK.exists(vk_entry => vk_entry.vk == q.vk) and q.b == true) implies {
                    f_sig_s.log.exists(log2 => {
                        match log2.msg {
                            | SignResponse(p) => {
                                and{
                                    log2.idx < log1.idx, // sign response comes before verify response
                                    p.cfg.sid == q.cfg.sid,
                                    p.m == q.m,
                                    p.sig != None,
                                    p.sig.unwrap() == q.sig,
                                    p.cfg.party == sid_state.VK.find(vk_entry => vk_entry.vk == q.vk).unwrap().party
                                }
                            }
                            | _ => false
                        }
                    })
                }
            }
            |_ => true
        }
    })

    val AllProps = all {
        OneVKperUser,
        VKUnique,
        VerConsistent,
        Completeness,
        Soundness   
    }

}