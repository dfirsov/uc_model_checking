module f_pki{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"

    import common.* from "../common"
    import f_pki_types.* from "./f_pki_types"

    var pki_s : PKI_System

    pure def pki_register_1(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_register_request_messages().map(p => {
            {
                post_state: pki.state,
                effects: Set(
                    PKI_LogMessage(RegisterRequest(p)),
                    PKI_AppendHandle(SIMRegisterRequestHold(p)),
                    PKI_UpdateInput(Set(SIMControl))
                    )
             }            
        })
    }    

    // TODO: rewrite with non-det if experiencing performance issues
    pure def pki_register_2(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_sim_control_messages().map(m => {
            pki.SIM_handles.pki_get_sim_register_request_hold_messages().map(p => {
                pki.params.SIM_ALL_CERTS.map(cert => {
                    {
                        post_state: pki.state,
                        effects: Set(
                            PKI_RemoveHandle(SIMRegisterRequestHold(p)),
                            PKI_UpdateInput(Set(SIMRegisterRequestRelease({cfg: p.cfg, vk: p.vk, cert: cert})))
                        )
                    }
                })
            }).flatten()
        }).flatten()
    }

    pure def pki_register_3(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_sim_register_request_release_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = pki.state.get(sid)

            if(sid_state.Reg.exists(r => r.party == party)){
                Set({
                    post_state: pki.state,
                    effects: Set(
                        PKI_UpdateInput(Set(RegisterResponse({cfg: p.cfg, vk: p.vk, cert: None}))),
                    )
                })   
            }else{
                Set({
                    post_state: 
                     pki.state.put(sid, {...sid_state,
                         Reg: sid_state.Reg.union(Set({party: party, vk: p.vk})),
                         Ver: sid_state.Ver.union(Set({party: party, vk: p.vk, cert: p.cert}))
                     }),
                    effects: Set( 
                        PKI_UpdateInput(Set(RegisterResponse({cfg: p.cfg, vk: p.vk, cert: Some(p.cert)}))),
                    )                    
                })
            }
        }).flatten()
    }

    pure def pki_retrieve_1(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_retrieve_request_messages().map(p => {
            {
                post_state: pki.state,
                effects: Set(
                    PKI_LogMessage(RetrieveRequest(p)),
                    PKI_AppendHandle(SIMRetrieveRequestHold(p)),
                    PKI_UpdateInput(Set(SIMControl))
                    )
             }            
        })
    }        

    pure def pki_retrieve_2(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_sim_control_messages().map(m => {
            pki.SIM_handles.pki_get_sim_retrieve_request_hold_messages().map(p => {
                {
                        post_state: pki.state,
                        effects: Set(
                            PKI_RemoveHandle(SIMRetrieveRequestHold(p)),
                            PKI_UpdateInput(Set(SIMRetrieveRequestRelease(p)))
                        )
                }
            })
        }).flatten()
    }

    pure def pki_retrieve_3(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_sim_retrieve_request_release_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = pki.state.get(sid)

            val vk_res = {
                val elem = sid_state.Reg.find(x => x.party == party)
                if (elem == None){
                    None
                }else{
                    Some(elem.unwrap().vk)
                }
            }

            Set({
                post_state: pki.state,
                effects: Set( 
                    PKI_UpdateInput(Set(RetrieveResponse({cfg: p.cfg, vk: vk_res})))
                )
            })
        }).flatten()
    }    

    pure def pki_cert_verify_1(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_cert_verify_request_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = pki.state.get(sid)

            val result = sid_state.Ver.exists(x => x.party == p.Q and x.vk == p.vk and x.cert == p.cert)            

            Set({
                post_state: pki.state,
                effects: Set(
                    PKI_LogMessage(CertVerifyRequest(p)),                   
                    PKI_UpdateInput(Set(CertVerifyResponse({
                        cfg: p.cfg,
                        Q: p.Q,
                        vk: p.vk,
                        cert: p.cert,
                        result: result
                    })))
                )
            })
        }).flatten()
    }

    // sim might want to give control back to the environment
    pure def pki_sim_loses_ctrl(crs: PKI_System) : Set[PKI_Transition] = {
        crs.input.pki_get_sim_control_messages().map(c =>  // sim must have control
            {
                post_state : crs.state,
                effects : Set(
                    PKI_UpdateInput(Set()),
                    )
            })
    }

    def pki_main_listener(crs: PKI_System) : Set[(PKI_Transition)] = {
        Set(
            pki_register_1(crs),
            pki_register_2(crs),
            pki_register_3(crs),

            pki_retrieve_1(crs),
            pki_retrieve_2(crs),
            pki_retrieve_3(crs),

            pki_cert_verify_1(crs),                

            pki_sim_loses_ctrl(crs)
        ).flatten()
    }

    def pki_process_transitions(sys: PKI_System, transition: PKI_Transition) : PKI_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => pki_apply_effect(a1,a2)))
    } 

    def pki_apply_effect(sys: PKI_System, eff: PKI_Effect): PKI_System = {
        match eff {
        | PKI_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg})
            }
        | PKI_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | PKI_AppendHandle(msg) => 
            {...sys,
                SIM_handles: sys.SIM_handles.union(Set(msg)),
            }
        | PKI_RemoveHandle(handle_to_remove) =>
            {...sys,
                SIM_handles: sys.SIM_handles.exclude(Set(handle_to_remove))
            }
        }
    }        

    action pki_step_with(q: (PKI_System) => Set[(PKI_Transition)]): bool = {
        all{
            val ts = q(pki_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    pki_s' = pki_process_transitions(pki_s, transition)
                }
            }
        }
    } 

    action pki_step : bool =  all {
        pki_step_with(pki_main_listener)
    }    

    pure def pki_initial(P: Set[Party], all_certs: Set[Certificate], SIDs: Set[SID]) : PKI_System = {
        {
            params : {
                P: P,
                SIDs: SIDs,

                SIM_ALL_CERTS: all_certs
            },
            state: SIDs.mapBy(sid => {
                Ver: Set(),
                Reg: Set(),
            }),
            input: Set(),
            nonce: 0, 
            log: Set(),
            SIM_handles: Set()
        }        
    }    

    action pki_init(P: Set[Party], all_certs: Set[Certificate], SIDs: Set[SID]) : bool =  all {
        pki_s' = pki_initial(P, all_certs, SIDs)
    }    


}