module f_atms_properties{
    
    import basicSpells.* from "../../spells/basicSpells"
    import f_atms_types.* from "./f_atms_types"
    import f_atms.* from "./f_atms"
    import f_atms_env.* from "./f_atms_env"
    import common.* from "../common"    
   

    val P1 = f_atms_s.log.size() <= 10
    val P2 = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().size() <= 2
    val P3 = f_atms_s.log.map(x => x.msg).atms_get_akey_response_messages().size() <= 2
    val P4 = f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().size() <= 2
    val P5 = f_atms_s.log.map(x => x.msg).atms_get_sign_response_messages().size() <= 2
    val P6 = f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().size() <= 2
    val P7 = f_atms_s.log.map(x => x.msg).atms_get_asign_response_messages().size() <= 2
    val P8 = f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().size() <= 2


    // Handles are properly managed (no orphaned holds)
    val NoOrphanedHolds = f_atms_s.handles.forall(h => {
        match h {
            | SIMAKeyHold(_) => true
            | SIMACheckHold(_) => true
            | SIMASignHold(_) => true
            | SIMAVerifyHold(_) => true
            | SIMGenHold(_) => true
            | SIMSignHold(_) => true
            | SIMVerifyHold(_) => true
            | _ => false
        }
    })
    // Input buffer has at most one message (as per design)
    val InputBound = f_atms_s.input.size() <= 1        
    

// no deadlock
temporal NeverStuck = {
  // WF is required because TLA+ semantics treat stuttering as a valid step even if not explicitly modeled
  step.weakFair(Set(f_atms_s)).implies ({
        not (eventually(
                always(
                    or {
                        f_atms_s.input != Set()
                    }
                )
        ))
    })
}

// Property: VK, VER, VKs, AVK, AVER are functional
val VK_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VK.forall(vk1 => {
        sid_state.VK.forall(vk2 => {
            (vk1.party == vk2.party) implies (vk1 == vk2)
        })
    })
})

val VER_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VER.forall(ver1 => {
        sid_state.VER.forall(ver2 => {
            and {
                ver1.vk == ver2.vk,
                ver1.m == ver2.m,
                ver1.sig == ver2.sig
            } implies (ver1 == ver2)
        })
    })
})

val VKs_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VKs.forall(vks1 => {
        sid_state.VKs.forall(vks2 => {
            (vks1.avk == vks2.avk) implies (vks1 == vks2)
        })
    })
})

val AVK_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.AVK.forall(avk1 => {
        sid_state.AVK.forall(avk2 => {
            (avk1.vks == avk2.vks) implies (avk1 == avk2)
        })
    })
})

val AVER_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.AVER.forall(aver1 => {
        sid_state.AVER.forall(aver2 => {
            and {
                aver1.avk == aver2.avk,
                aver1.m == aver2.m,
                aver1.sig == aver2.sig
            } implies (aver1 == aver2)
        })
    })
})

val AllFunctional = all {
    VK_Functional,
    VER_Functional,
    //AVK_Functional, // BUG: Not functional
    AVER_Functional,
    VKs_Functional
}

// Property: VK, VKs, AVK are injective (for example, if VK[P] and VK[Q] defined and VK[P] = VK[Q] then P = Q)
val VK_Injective = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VK.forall(vk1 => {
        sid_state.VK.forall(vk2 => {
            (vk1.vk == vk2.vk) implies (vk1.party == vk2.party)
        })
    })
})

// BUG: Not injective?
val VKs_Injective = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VKs.forall(vks1 => {
        sid_state.VKs.forall(vks2 => {
            (vks1.vks == vks2.vks and vks1.vks != List()) implies (vks1.avk == vks2.avk)
        })
    })
})

val AVK_Injective = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.AVK.forall(avk1 => {
        sid_state.AVK.forall(avk2 => {
            and {
                avk1.avk != None,
                avk2.avk != None,
                avk1.avk == avk2.avk,
            } implies (avk1.vks == avk2.vks)
        })
    })
})



val AllInjective = all {
    VK_Injective,
    VKs_Injective,
    AVK_Injective
}

// TODO: Any other properties/invariants connecting VK, VER, VKs, AVK, AVER, Signers, CVK?


// Determinism of Gen, acheck, ver, aver properties (requests with same input lead to same output)
// go through a log: if there are two responses with the same inputs then their outputs must be the same

// TODO: check the preconditions, for example should Acheck be deterministic across parties, or not?
val Gen_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
        }) implies (msg1.vk == msg2.vk)
    })
})

val ACheck_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
            msg1.vks == msg2.vks,
            msg1.avk == msg2.avk,
        }) implies (msg1.b == msg2.b)
    })
})

val Verify_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
            msg1.vk == msg2.vk,
            msg1.m == msg2.m,
            msg1.sig == msg2.sig,
        }) implies (msg1.b == msg2.b)
    })
})

val AVerify_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
            msg1.avk == msg2.avk,
            msg1.m == msg2.m,
            msg1.sig == msg2.sig,
        }) implies (msg1.b == msg2.b)
    })
})

val AllDeterministic = all {
    Gen_Deterministic,
    ACheck_Deterministic,
    Verify_Deterministic,
    AVerify_Deterministic
}


// ACheck correctness: Any vks that have been aggregated to an avk ≠ None will pass ACheck
// for all AKey responses in the log, if avk != None then ACheck of that avk and vks must be true

val ACheck_Correctness = f_atms_s.log.map(x => x.msg).atms_get_akey_response_messages().forall(akey_resp => {
    match akey_resp.avk {
        | None => true
        | Some(avk_val) => {
            f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().forall(acheck_resp => {
                (and {
                    akey_resp.cfg.nonce < acheck_resp.cfg.nonce, 
                    acheck_resp.cfg.sid == akey_resp.cfg.sid,
                    acheck_resp.vks == akey_resp.vks,
                    acheck_resp.avk == avk_val
                }) implies (acheck_resp.b == true)
            })
        }
    }
})

//Sig Correctness: Any signature generated by an honest party will later pass verification. (As in F-Sig.) 
// for all sig responses in the log, if party is honest then Verify response of that sig must be true
val Sig_Correctness = f_atms_s.log.map(x => x.msg).atms_get_sign_response_messages().forall(sign_resp => {
    match sign_resp.sig {
        | None => true
        | Some(sig_val) => {
            if (not (f_atms_s.params.C.contains(sign_resp.cfg.party))) {
                // Party is honest, find their VK from Gen responses
                val gen_responses = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().filter(gen => 
                    and {
                        gen.cfg.party == sign_resp.cfg.party,
                        gen.cfg.sid == sign_resp.cfg.sid
                    }
                )
                gen_responses.forall(gen_resp => {
                    // For this VK, check all Verify responses
                    f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().forall(verify_resp => {
                        (and {
                            verify_resp.cfg.sid == sign_resp.cfg.sid,
                            verify_resp.vk == gen_resp.vk,
                            verify_resp.m == sign_resp.m,
                            verify_resp.sig == sig_val
                        }) implies (verify_resp.b == true)
                    })
                })
            } else {
                true  // Not an honest party, no constraint
            }
        }
    }
})

}