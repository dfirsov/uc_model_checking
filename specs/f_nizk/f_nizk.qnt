module f_nizk{

    import common.* from "../common"
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"    
    import f_nizk_types.* from "./f_nizk_types"
 
    var f_nizk_s : NIZK_System

    // SIM gives control back to the environment
    pure def nizk_sim_loses_ctrl(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_sim_control_messages().map(c =>  // sim must have control
            {   
                post_state : nizk.state,
                effects : Set( 
                    NIZK_UpdateInput(Set())
                )  
            })
    }

    pure def nizk_prove_1(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_prove_request_messages().map(m => {
            if(nizk_access_check(m.cfg, nizk.params)) {
                val sid = m.cfg.sid
                val party = m.cfg.party
                val sid_state = nizk.state.get(sid)
            
                if(not (nizk_Rel_sat(nizk,sid,m.x,m.w))) { // (statement, witness) are not in the relation
                    {
                        post_state : nizk.state,
                        effects : Set( 
                            NIZK_LogMessage(ProveRequest(m)),
                            NIZK_UpdateInput(Set(ProveResponse({cfg: m.cfg, x: m.x, w: m.w, pi: None})))
                        )
                    }
                }else{
                    {
                        post_state: nizk.state,
                        effects: Set(
                            NIZK_LogMessage(ProveRequest(m)),
                            NIZK_UpdateInput(Set(SIMControl)),
                            NIZK_AppendHandle(SIMProveHold({cfg:m.cfg, x:m.x, w:m.w}))
                        )
                    }
                }
            }else{
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set()),
                    )
                }
            }
            })
    }

    pure def nizk_prove_2(nizk: NIZK_System, sim_pi: NIZK_Proof) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_sim_control_messages().map(p => {
            nizk.handles.nizk_get_sim_prove_hold_messages().map(q => {
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set(SIMProveRelease({cfg: q.cfg, x: q.x, w: q.w, pi: sim_pi}))),
                        NIZK_RemoveHandle(SIMProveHold(q))
                    )
                }
            })
        }).flatten()
    }

    action nizk_prove_2_act : bool = all {
        f_nizk_s.input.nizk_get_sim_control_messages().size() > 0,
        f_nizk_s.handles.nizk_get_sim_prove_hold_messages().size() > 0,
        nondet sim_pi = f_nizk_s.params.ALL_PROOFS.oneOf()
        nondet t = nizk_prove_2(f_nizk_s, sim_pi).oneOf()
        f_nizk_s' =  nizk_process_transitions(f_nizk_s, t)
    } 

    pure def nizk_prove_3(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_sim_prove_release_messages().map(p => {
            val party = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = nizk.state.get(sid)

            def san_predicate = (pi) => {
                or{
                    sid_state.Ver.exists(e => e.x == p.x and e.pi == pi and e.b == false)
                }
            }

            val maybe_new_pi = if(san_predicate(p.pi)){
                nizk.params.ALL_PROOFS.find(x => not (san_predicate(x)))
            }else{
                Some(p.pi)
            }

            if(maybe_new_pi == None){ // we exhausted all possible proofs
                {
                    post_state: nizk.state,
                    effects: Set(
                        NIZK_UpdateInput(Set())
                    )
                }
            }else{
                val new_pi = maybe_new_pi.unwrap()
                {
                    post_state: nizk.state.set(sid, {...sid_state,
                        Ver: sid_state.Ver.setAdd({x: p.x, pi: new_pi, b: true})
                    }),
                    effects: Set(
                        NIZK_UpdateInput(Set(ProveResponse({cfg: p.cfg, x: p.x, w: p.w, pi: Some(new_pi)})))
                    )
                }
            }
        })
    }

    pure def nizk_verify_1(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_verify_request_messages().map(m => {
            if(nizk_access_check(m.cfg, nizk.params)) {
                val sid = m.cfg.sid
                val party = m.cfg.party
                val sid_state = nizk.state.get(sid)
                {
                    post_state: nizk.state,
                    effects: Set(
                        NIZK_LogMessage(VerifyRequest(m)),
                        NIZK_UpdateInput(Set(SIMControl)),
                        NIZK_AppendHandle(SIMVerifyHold(m))
                    )
                }
            }else{
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set()),
                    )
                }
            }
        })
    }    

    pure def nizk_verify_2(nizk: NIZK_System, sim_w: NIZK_Witness) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_sim_control_messages().map(p => {
            nizk.handles.nizk_get_sim_verify_hold_messages().map(q => {
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set(SIMVerifyRelease({cfg: q.cfg, x: q.x, pi: q.pi, w: sim_w}))),
                        NIZK_RemoveHandle(SIMVerifyHold(q))
                    )
                }
            })
        }).flatten()
    }    

    action nizk_verify_2_act : bool = all {
        f_nizk_s.input.nizk_get_sim_control_messages().size() > 0,
        f_nizk_s.handles.nizk_get_sim_verify_hold_messages().size() > 0,
        nondet sim_w = f_nizk_s.params.ALL_WITNESSES.oneOf()
        nondet t = nizk_verify_2(f_nizk_s, sim_w).oneOf()
        f_nizk_s' =  nizk_process_transitions(f_nizk_s, t)
    }      

    pure def nizk_verify_3(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.nizk_get_sim_verify_release_messages().map(p => {
            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = nizk.state.get(sid)

            if(sid_state.Ver.exists(e => e.x == p.x and e.pi == p.pi)){
                val old_b = sid_state.Ver.find(e => e.x == p.x and e.pi == p.pi).unwrap().b
                {
                    post_state: nizk.state,
                    effects: Set( 
                        NIZK_UpdateInput(Set(VerifyResponse({cfg:p.cfg, x:p.x, pi: p.pi, b: old_b})))
                    )
                }
            }else{
                val new_b = nizk_Rel_sat(nizk, sid, p.x, p.w)
                {
                    post_state: nizk.state.set(sid, {...sid_state,
                        Ver: sid_state.Ver.setAdd({x: p.x, pi: p.pi, b: new_b})
                    }),
                    effects: Set(
                        NIZK_UpdateInput(Set(VerifyResponse({cfg:p.cfg, x:p.x, pi: p.pi, b: new_b})))
                    )
                }
            }
        })
    }    


    def nizk_process_transitions(sys: NIZK_System, transition: NIZK_Transition) : NIZK_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => nizk_apply_effect(a1,a2)))
    } 

   pure def nizk_main_listener(nizk: NIZK_System) : (Set[NIZK_Transition]) = {
        Set(
            nizk_prove_1(nizk),
           //nizk_prove_2(nizk),
            nizk_prove_3(nizk),

            nizk_verify_1(nizk),
           //nizk_verify_2(nizk),
            nizk_verify_3(nizk),

            nizk_sim_loses_ctrl(nizk),
            
        ).flatten()
    }            

    action nizk_step_with(q: (NIZK_System) => Set[(NIZK_Transition)]): bool = {
        all{
            val ts = q(f_nizk_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_nizk_s' = nizk_process_transitions(f_nizk_s, transition),
                }
            }
        }
    }  

    action nizk_step : bool =  any {
        nizk_step_with(nizk_main_listener),
        nizk_verify_2_act,
        nizk_prove_2_act
    }  

   action nizk_init(P: Set[Party], C: Set[Party],SIDs: Set[SID], ALL_PROOFS: Set[NIZK_Proof], ALL_WITNESSES: Set[NIZK_Witness], Rel: NIZK_Relation) : bool =  all {
        f_nizk_s' = {
            params : {
                P: P,
                C: C,
                SIDs: SIDs,

                ALL_PROOFS: ALL_PROOFS,
                ALL_WITNESSES: ALL_WITNESSES,
            },
            state : SIDs.mapBy(sid => {
                Ver: Set(),
                Rel: Rel,
            }),
            input : Set(),
            nonce : 0,
            log : Set(),
            handles : Set(),
        }
    }
}