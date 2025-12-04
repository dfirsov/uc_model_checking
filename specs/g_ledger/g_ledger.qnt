module g_ledger{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "./g_ledger_types"
    

    var g_ledger_s : LED_System

    // SIM gives control back to the environment
    pure def led_sim_loses_ctrl(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_sim_control_messages().map(c =>  // sim must have control
            {   
                post_state : led.state,
                effects : Set( 
                    LED_UpdateInput(Set()),
                    )  
            }).map(t => (t, gc_id_trans(clock)))
    }

    // TODO: add P and sid checks
    pure def led_submit_1(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.input.led_get_submit_request_messages().map(p => {

            val ledger_transition = {
                post_state: ledger.state,
                effects: Set(
                    LED_AppendHandle(SubmitTimeRequestHold(p)),
                    LED_LogMessage(SubmitRequest(p)),
                    LED_UpdateInput(Set())
                )
            }

            val gc_transition = {
                post_state: clock.state, 
                effects: Set(GC_UpdateInput(Set(TimeRequest({nonce: p.cfg.nonce, party: {party: p.cfg.party, sid: p.cfg.sid}}))))
            }

            (ledger_transition, gc_transition)

        })
    }

    pure def led_submit_2(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.handles.led_get_submit_time_request_hold_messages().map(p => {
          clock.input.get_time_response_messages().map(q => {

            if(p.cfg.nonce == q.nonce){
                val t = q.time
                val eID = ledger.state.eID + 1
                val buffer = ledger.state.buffer.put(eID, ({tx: p.tx, time:t}))

                val ledger_transition = {
                    post_state: {...ledger.state, buffer: buffer, eID: eID},
                    effects: Set(
                        LED_RemoveHandle(SubmitTimeRequestHold(p)),
                        LED_AppendHandle(SubmitSIMHold({cfg: p.cfg, tx: p.tx, t: t, eID: eID})),
                        LED_UpdateInput(Set(SIMControl))
                    )
                }

                val gc_transition = {
                    post_state: clock.state,
                    effects: Set(
                        GC_UpdateInput(Set())
                    )
                }

                Set((ledger_transition, gc_transition))
            }else{
                Set()
            }
          }).flatten()
        }).flatten()        
    }

    // simulator
    pure def led_submit_3(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.handles.led_get_submit_sim_hold_messages().map(p => {
            ledger.input.led_get_sim_control_messages().map(q => {
                
                {
                    post_state: ledger.state,
                    effects: Set(
                        LED_RemoveHandle(SubmitSIMHold(p)),
                        LED_UpdateInput(Set(SubmitSIMRelease(p)))
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))        
    }

    pure def led_submit_4(led : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition) ] = {
        led.input.led_get_submit_sim_release_messages().map(p => {
            {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set(SubmitResponse(p))),
                )
            }
        }).map(t => (t,gc_id_trans(clock)))
    }

    // TODO: access control
   pure def led_ok_1(led : LED_System, gc: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_ok_request_messages().map(msg => {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val led_transition = {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set()),
                    LED_AppendHandle(OKRequestWait(msg)),
                    LED_LogMessage(OKRequest(msg))
                )
            }

            val gc_transition = {
                post_state: gc.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({nonce: nonce, party:{party: P, sid:sid}})))
                )
            }

            (led_transition, gc_transition)
        })
    }     


    pure def led_ok_2(led : LED_System, gc: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.handles.led_get_ok_request_wait_messages().map(msg => {
            gc.input.get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val led_transition = {
                        post_state: led.state,
                        effects: Set(
                            LED_UpdateInput(Set(OKResponse(msg))),
                            LED_RemoveHandle(OKRequestWait(msg))
                        )
                    }

                    val gc_transition = {
                        post_state: gc.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((led_transition, gc_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }           


    // TODO: add access control
    pure def led_read_1(led : LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_read_request_messages().map(p => {
            {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set(SIMControl)),
                    LED_AppendHandle(ReadSIMHold(p)),
                    LED_LogMessage(ReadRequest(p))
                )
            }
        }).map(t => (t, gc_id_trans(clock)))
    }

    // TODO: redo with apalache::generate
    pure def led_read_2(ledger : LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.handles.led_get_read_sim_hold_messages().map(p => {
            ledger.input.led_get_sim_control_messages().map(q => {
                
                Set({
                    post_state: ledger.state,
                    effects: Set(
                        LED_RemoveHandle(ReadSIMHold(p)),
                        LED_UpdateInput(Set(ReadSIMRelease({cfg:p.cfg, I: List(1,2,3,5,6,7,8,9,10), ptr_prim: 3}))) 
                    )
                })
            }).flatten()
        }).flatten().map(t => (t, gc_id_trans(clock)))        
    }

    pure def led_read_3(ledger: LED_System, clock: GC_System) : Set[(LED_Transition,GC_Transition)] = {
        ledger.input.led_get_read_sim_release_messages().map(p => {

            val led_transition = {
                post_state: ledger.state,
                effects: Set(
                    LED_UpdateInput(Set()),
                    LED_AppendHandle(ReadTimeRequestHold({cfg: p.cfg, I: p.I, ptr_prim: p.ptr_prim}))
                )
            }

            val gc_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(TimeRequest({nonce: p.cfg.nonce, party: {party: p.cfg.party, sid: p.cfg.sid}})))
                )
            }

            (led_transition, gc_transition)
        })
    }

    pure def led_read_4(ledger: LED_System, clock: GC_System) : Set[(LED_Transition,GC_Transition)] = {
        ledger.handles.led_get_read_time_request_hold_messages().map(p => {
          clock.input.get_time_response_messages().map(q => {

            if(p.cfg.nonce == q.nonce){
                val t = q.time
                val I = p.I
                val ptr_prim = p.ptr_prim
                val P = p.cfg.party

                val X = led_san(ledger, I, t, ptr_prim)
                val I = X._1
                val ptr_prim = X._2

                val ptr = ledger.state.ptr.put(P, {
                    val ptrP = if (ledger.state.ptr.has(P)) ledger.state.ptr.get(P) else ptr_prim // todo check
                    if(ptrP < ptr_prim) ptr_prim else ptrP
                })                

                val loop = I.foldl({v: ledger.state.Val, I: I, b: ledger.state.buffer, l:ledger.state.log}, (x, i) => {
                    val y = led_valid(x.v, x.b.get(i).tx)
                    if(y.valid){
                        {v:y.val_state, I: x.I, b: x.b, l:x.l.append({tx: x.b.get(i).tx, time: t})}
                    }else{
                        {v:y.val_state, I: x.I, b: x.b.mapRemove(i), l: x.l}
                    }
                })

                val ptrP_2 = if (ptr.get(P) < 0) ptr.set(P, 0) else ptr
                val ptrP_3 = if (ptrP_2.get(P) >= loop.l.length()) ptrP_2.set(P, loop.l.length() - 1) else ptrP_2
                //val result = loop.l.slice(0,ptrP_3.get(P)).listMap(b => b.tx) // TODO: check off-by-one

                val led_transition = {
                    post_state: {...ledger.state, 
                        buffer: loop.b,
                        log: loop.l,
                        ptr: ptrP_3,
                        Val: loop.v
                    },
                    effects: Set(
                        LED_RemoveHandle(ReadTimeRequestHold(p)),
                        LED_UpdateInput(Set(ReadResponse({cfg: p.cfg, result: List()})))
                    )
                }

                val gc_transition = {
                    post_state: clock.state,
                    effects: Set(
                        GC_UpdateInput(Set())
                    )
                }

                Set((led_transition, gc_transition))
            }else{
                Set()
            }
          }).flatten()
        }).flatten()
    }

    pure def led_san(ledger: LED_System, I : List[Pointer], t : Time, ptr_prim: Pointer) : (List[Pointer], Pointer) = {

        val newI = I.listFilter(i => ledger.state.buffer.has(i))

        val oldIs = ledger.state.buffer.keys().filter(i => {
        
             and {
                not (I.listHas(i)),
                t - ledger.state.buffer.get(i).time >= ledger.params.delta_latency
             }
        })

        val oldIsList = oldIs.toList(intCompare)

        val I = newI.concat(oldIsList)

        val maxPtr = ledger.state.log.indices().filterMap(idx => {
            val b = ledger.state.log[idx]
            if(t - b.time > ledger.params.delta_slack) {
                Some(idx)
            } else {
                None
            }
        }).union(Set(ptr_prim)).setMax(ptr_prim)

        (I, maxPtr) 
    }
     
    // TODO: either adhoc function, or tx paired with validity flag
    pure def led_valid(vallidator: LED_ValidatorState, block: Block): {val_state: LED_ValidatorState, valid: bool} = {
        {val_state: vallidator, valid: true} 
    }
        

   def led_main_listener(led: LED_System, clock: GC_System) : (Set[(LED_Transition, GC_Transition)]) = {
        Set(
            led_ok_1(led, clock),
            led_ok_2(led, clock),
            led_submit_1(led, clock),
            led_submit_2(led, clock),
            led_submit_3(led, clock),
            led_submit_4(led, clock),
            led_read_1(led, clock),
            led_read_2(led, clock),
            led_read_3(led, clock),
            led_read_4(led, clock),
            led_sim_loses_ctrl(led, clock)
        ).flatten()
    }    


    def led_apply_effect(sys: LED_System, eff: LED_Effect): LED_System = {
        match eff {
        | LED_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: gc_s.state.TIME, handles: sys.handles})
            }
        | LED_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | LED_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | LED_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    def led_process_transitions(sys: LED_System, transition: LED_Transition) : LED_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => led_apply_effect(a1,a2)))
    }     

    action led_gc_step_with(q: (LED_System, GC_System) => Set[(LED_Transition, GC_Transition)]): bool = {
        all{
            val ts = q(g_ledger_s, gc_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, transition._1),
                    gc_s' = gc_process_transitions(gc_s, transition._2)
                }
            }
        }
    }    

    action led_step : bool =  all {
        led_gc_step_with(led_main_listener)
    }    

    pure def led_initial(P: Set[Party], C: Set[Party], SIDs: Set[SID], delta_latency: Time, delta_slack: Time, genesis: TX, Val: LED_ValidatorState) : LED_System = {
        {
            params : {
                P : P,
                C : C,
                SIDs: SIDs,
                delta_latency: delta_latency,
                delta_slack: delta_slack,
                genesis: genesis,
            },
            state : 
                {
                    buffer : Map(),
                    eID : 0,
                    log: List({tx:genesis,time:0}),
                    ptr: Map(),
                    Val: Val 
                },
            input : Set(),
            nonce : 0,      
            log : Set(),
            handles : Set()
        }        
    }

    action led_init(P: Set[Party], C: Set[Party], SIDs: Set[SID], delta_latency: Time, delta_slack: Time, genesis: TX, Val: LED_ValidatorState) : bool =  all {
        g_ledger_s' = led_initial(P, C, SIDs, delta_latency, delta_slack, genesis, Val)
    }          

}