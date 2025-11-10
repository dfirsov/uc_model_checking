module g_clock_ideal{

    import basicSpells.* from "../../spells/basicSpells"
    import uc.* from "./uc"
    import g_clock_types.* from "./g_clock_types"


    // mutable variable represents the system state
    var s : System

    pure def register(sys: System) :  Set[Transition] = { 
        sys.input.get_register_request_messages().map(msg => 
        {
            post_state : {...sys.state, 
                Reg : sys.state.Reg.union(Set(msg.party))
            },
            effects : Set(
                LogMessage(RegisterRequest({nonce: msg.nonce, party: msg.party})),
                UpdateInput(Set(RegisterResponse({nonce: msg.nonce, party: msg.party}))),
            )
        }
        )
    }

    pure def time(sys: System) : Set[Transition] = {
        sys.input.get_time_request_messages().map(msg =>
        {
            post_state : sys.state,
            effects : Set(
                LogMessage(TimeRequest({nonce: msg.nonce, party: msg.party})),
                UpdateInput(Set(TimeResponse({nonce: msg.nonce, party: msg.party, time: sys.state.TIME}))),
            )
        }
        )
    }

    pure def okay_1(sys: System) : Set[Transition] = {
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
                        LogMessage(OkayRequest({nonce: msg.nonce, party: msg.party})),
                        UpdateInput(Set(SIMOkayHold({nonce: msg.nonce, party: msg.party}))),
                    )
                }
            } else {
                // Party not registered - send control message
                {
                    post_state : sys.state,
                    effects : Set(
                        LogMessage(OkayRequest({nonce: msg.nonce, party: msg.party})),
                        UpdateInput(Set()), // control goes to env
                    )
                }
            }
        })
    }

    pure def okay_2(sys: System) : Set[Transition] = {
        sys.input.get_sim_okay_release_messages().map(msg =>
        {
            post_state : sys.state,
            effects : Set(
                UpdateInput(Set(OkayResponse({nonce: msg.nonce, party: msg.party}))),
            )
        }
        )
    }

    pure def sim_okay_hold_reqs(sys: System) : Set[Transition] = {
        sys.input.get_sim_okay_hold_messages().map(msg => {
            val hold_msg = SIMOkayRelease({nonce: msg.nonce, party: msg.party})
            {
                post_state : sys.state,
                effects : Set(
                    AppendHandle(hold_msg),
                    UpdateInput(Set(SIMControl)),
                )
            }
        })
    }

    pure def sim_process_hold_reqs(sys: System) : Set[Transition] = {
        if(sys.SIM_handles.size() > 0 and sys.input.contains_sim_control()) {
            // Option 1: Release one of the stored hold requests
            val release_trans = sys.SIM_handles.map(release_request => {
                {
                    post_state : sys.state,
                    effects : Set(
                        UpdateInput(Set(release_request)),
                        RemoveHandle(release_request),
                    )
                }
            })
            
            // Option 2: Give control back to ENV (empty input)
            val give_control_trans = Set({
                post_state : sys.state,
                effects : Set(
                    UpdateInput(Set()),
                )
            })
            
            release_trans.union(give_control_trans)
        } else {
            Set()
        }
    }

    pure def main_listener(sys : System) : Set[Transition] = {
        Set(
            register(sys),
            time(sys),
            okay_1(sys),
            sim_okay_hold_reqs(sys),
            sim_process_hold_reqs(sys),
            okay_2(sys),
        ).flatten()
    }  

    pure def process_transitions(sys: System, transition: Transition) : System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => apply_effect(a1,a2)))
    } 

    action step : bool =  { 
        nondet transition = main_listener(s).oneOf()
        s' = process_transitions(s, transition)
    }

    action init(C: Set[Party], time : int, Reg: Set[PartySID]): bool = {
        s' = {
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
            SIM_handles : Set()
        }
    }
    
}