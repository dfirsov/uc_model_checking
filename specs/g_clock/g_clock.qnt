module g_clock{

    import basicSpells.* from "../../spells/basicSpells"
    import g_clock_types.* from "./g_clock_types"
    import common.* from "../common"

    // mutable variable represents the system state
    var gc_s : GC_System


    pure def gc_register(sys: GC_System) :  Set[GC_Transition] = { 
        sys.input.get_register_request_messages().map(msg => 
        {
            post_state : {...sys.state, 
                Reg : sys.state.Reg.union(Set(msg.party))
            },
            effects : Set(
                GC_LogMessage(RegisterRequest({nonce: msg.nonce, party: msg.party})),
                GC_UpdateInput(Set(RegisterResponse({nonce: msg.nonce, party: msg.party}))),
            )
        }
        )
    }

    pure def gc_time(sys: GC_System) : Set[GC_Transition] = {
        sys.input.get_time_request_messages().map(msg =>
        {
            post_state : sys.state,
            effects : Set(
                GC_LogMessage(TimeRequest({nonce: msg.nonce, party: msg.party})),
                GC_UpdateInput(Set(TimeResponse({nonce: msg.nonce, party: msg.party, time: sys.state.TIME}))),
            )
        }
        )
    }

    pure def gc_okay_1(sys: GC_System) : Set[GC_Transition] = {
        sys.input.get_okay_request_messages().map(msg => {
            if(sys.state.Reg.contains(msg.party)) {
                // Check if all registered parties are ready (including this one)
                val flag = sys.state.Reg.forall(rp => or {
                     sys.state.ok.contains(rp), 
                     rp == msg.party, 
                     msg.party.party.in(sys.params.C)
                })
                val new_time = if (flag) sys.state.TIME + 1 else sys.state.TIME
                // for whatever reason we are keeping corrupted parties in the ok set
                val new_ok = if (flag) Set() else sys.state.ok.setAdd(msg.party)
                
                {
                    post_state : {...sys.state, 
                        ok: new_ok,
                        TIME: new_time
                    },
                    effects : Set(
                        GC_LogMessage(OkayRequest({nonce: msg.nonce, party: msg.party})),
                        GC_UpdateInput(Set(SIMOkayHold({nonce: msg.nonce, party: msg.party}))),
                    )
                }
            } else {
                // Party not registered - send control message
                {
                    post_state : sys.state,
                    effects : Set(
                        GC_LogMessage(OkayRequest({nonce: msg.nonce, party: msg.party})),
                        GC_UpdateInput(Set()), // control goes to env
                    )
                }
            }
        })
    }

    pure def gc_okay_2(sys: GC_System) : Set[GC_Transition] = {
        sys.input.get_sim_okay_hold_messages().map(msg => {
            val hold_msg = SIMOkayRelease({nonce: msg.nonce, party: msg.party})
            {
                post_state : sys.state,
                effects : Set(
                    GC_AppendHandle(hold_msg),
                    GC_UpdateInput(Set(GC_SIMControl)),
                )
            }
        })
    }

    pure def gc_okay_4(sys: GC_System) : Set[GC_Transition] = {
        sys.input.get_sim_okay_release_messages().map(msg =>
        {
            post_state : sys.state,
            effects : Set(
                GC_UpdateInput(Set(OkayResponse({nonce: msg.nonce, party: msg.party}))),
            )
        }
        )
    }



    pure def gc_okay_3(sys: GC_System) : Set[GC_Transition] = {
        if(sys.SIM_handles.size() > 0 and sys.input.contains_sim_control()) {
            // Option 1: Release one of the stored hold requests
            val release_trans = sys.SIM_handles.map(release_request => {
                {
                    post_state : sys.state,
                    effects : Set(
                        GC_UpdateInput(Set(release_request)),
                        GC_RemoveHandle(release_request),
                    )
                }
            })
            
            // Option 2: Give control back to ENV (empty input)
            // val give_control_trans = Set({
            //     post_state : sys.state,
            //     effects : Set(
            //         GC_UpdateInput(Set()),
            //     )
            // })
            
            //release_trans.union(give_control_trans)
            release_trans
        } else {
            if (sys.input.contains_sim_control()) {
                // No stored hold requests - give control back to ENV
                Set({
                    post_state : sys.state,
                    effects : Set(
                        GC_UpdateInput(Set()),
                    )
                })
            } else {
                Set()
            }
        }
    }

    pure def gc_main_listener(sys : GC_System) : Set[GC_Transition] = {
        Set(
            gc_register(sys),
            gc_time(sys),
            gc_okay_1(sys),
            gc_okay_2(sys),
            gc_okay_3(sys),
            gc_okay_4(sys),
        ).flatten()
    }  

    pure def gc_process_transitions(sys: GC_System, transition: GC_Transition) : GC_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => g_clock_apply_effect(a1,a2)))
    } 

    action gc_step_with(q: (GC_System) => Set[(GC_Transition)]): bool = {
        nondet transition = q(gc_s).oneOf()
        gc_s' = gc_process_transitions(gc_s, transition)        
    }   

    action gc_step : bool =  { 
        gc_step_with(gc_main_listener)
    }

    action gc_init(C: Set[Party], time : int, Reg: Set[PartySID]): bool = {
        gc_s' = {
            params: {
                C : C,
            },
            state : {
                TIME : time,
                Reg : Reg,
                ok  : Set(),
            },
            nonce : 0,
            input : Set(),
            log : Set(),
            SIM_handles : Set(),
        }
    }

    action gc_stutter : bool = {
        gc_s' = gc_s
    }

 
    
}