module g_ledger{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "./g_ledger_types"
    

    var g_ledger_s : LED_System

    pure def submit_1(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.input.led_get_submit_request_messages().map(p => {

            val ledger_transition = {
                post_state: ledger.state,
                effects: Set(
                    LED_AppendHandle(SubmitTimeRequestHold(p)),
                    LED_UpdateInput(Set())
                )
            }

            val gc_transition = {
                post_state: clock.state, // | TimeRequest({nonce: Nonce, party: PartySID})
                effects: Set(GC_UpdateInput(Set(TimeRequest({nonce: p.cfg.nonce, party: {party: p.cfg.party, sid: p.cfg.sid}}))))
            }

            Set((ledger_transition, gc_transition))

        }).flatten()        
    }

    pure def submit_2(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
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
    pure def submit_3(ledger : LED_System) : Set[LED_Transition] = {
        ledger.handles.led_get_submit_sim_hold_messages().map(p => {
            ledger.input.led_get_sim_control_messages().map(q => {
                
                Set({
                    post_state: ledger.state,
                    effects: Set(
                        LED_RemoveHandle(SubmitSIMHold(p)),
                        LED_UpdateInput(Set(SubmitSIMRelease(p)))
                    )
                })
            }).flatten()
        }).flatten()        
    }

    pure def submit_4(ledger : LED_System) : Set[LED_Transition] = {
        ledger.input.led_get_submit_sim_release_messages().map(p => {
            {
                post_state: ledger.state,
                effects: Set(
                    LED_UpdateInput(Set(SubmitResponse(p)))
                )
            }
        })
    }

   pure def dif_ok_1(dif : LED_System, gc: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        dif.input.led_get_ok_request_messages().map(msg => {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val dif_transition = {
                post_state: dif.state,
                effects: Set(
                    LED_UpdateInput(Set()),
                    LED_AppendHandle(OKRequestWait(msg))
                )
            }

            val gc_transition = {
                post_state: gc.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({nonce: nonce, party:{party: P, sid:sid}})))
                )
            }

            (dif_transition, gc_transition)
        })
    }     


    pure def dif_ok_2(dif : LED_System, gc: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        dif.handles.led_get_ok_request_wait_messages().map(msg => 
        {
            gc.input.get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val dif_transition = {
                        post_state: dif.state,
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
                    Set((dif_transition, gc_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }           


    pure def read_1(ledger : LED_System) : Set[LED_Transition] = {
        ledger.input.led_get_read_request_messages().map(p => {
            {
                post_state: ledger.state,
                effects: Set(
                    LED_UpdateInput(Set(SIMControl)),
                    LED_AppendHandle(ReadSIMHold(p))
                )
            }
        })
    }

    pure def read_2(ledger : LED_System) : Set[LED_Transition] = {
        ledger.handles.led_get_read_sim_hold_messages().map(p => {
            ledger.input.led_get_sim_control_messages().map(q => {
                
                Set({
                    post_state: ledger.state,
                    effects: Set(
                        LED_RemoveHandle(ReadSIMHold(p)),
                        LED_UpdateInput(Set(ReadSIMRelease({cfg:p.cfg, I: List(), ptr_prim: 42}))) // TODO redo with apalache::generate
                    )
                })
            }).flatten()
        }).flatten()        
    }

    pure def read_3(ledger: LED_System, clock: GC_System) : Set[(LED_Transition,GC_Transition)] = {
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

            Set((led_transition, gc_transition))
        }).flatten()
    }

    pure def read_4(ledger: LED_System, clock: GC_System) : Set[(LED_Transition,GC_Transition)] = {
        ledger.handles.led_get_read_time_request_hold_messages().map(p => {
          clock.input.get_time_response_messages().map(q => {

            if(p.cfg.nonce == q.nonce){
                val t = q.time
                val I = p.I
                val ptr_prim = p.ptr_prim
                val P = p.cfg.party

                val X = led_san(ledger, I, t, ptr_prim)
                val newI = X._1
                val newPtr_prim = X._2

                val ptrP = ledger.state.ptr.get(P)
                val maxval = if(ptrP < newPtr_prim) newPtr_prim else ptrP
                val ptr = ledger.state.ptr.put(P, maxval)                


                val Z = newI.foldl({v: ledger.state.Val, I: newI, b: ledger.state.buffer, l:ledger.state.log}, (x, i) => {
                    val y = led_valid(x.v, x.b.get(i).tx)
                    if(y.valid){
                        {v:y.val_state, I: x.I, b: x.b, l:x.l.append({tx: x.b.get(i).tx, time: t})}
                    }else{
                        {v:y.val_state, I: x.I, b: x.b.mapRemove(i), l: x.l}
                    }
                })

                val ptr_2 = if (ptr.get(P) < 0) ptr.set(P, 0) else ptr
                val ptr_3 = if (ptr_2.get(P) >= Z.l.length()) ptr_2.set(P, Z.l.length() - 1) else ptr_2

                val led_transition = {
                    post_state: {...ledger.state, 
                        buffer: Z.b,
                        log: Z.l,
                        ptr: ptr_3,
                        Val: Z.v
                    },
                    effects: Set(
                        LED_RemoveHandle(ReadTimeRequestHold(p)),
                        LED_UpdateInput(Set(ReadResponse({cfg: p.cfg, result: Z.l.slice(0,ptr_3.get(P)).listMap(b => b.tx) })))
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

        val newIs = I.listFilter(i => ledger.state.buffer.has(i))

        val oldIs = ledger.state.buffer.keys().filter(i => {
        
             and {
                not (I.listHas(i)),
                t - ledger.state.buffer.get(i).time >= ledger.params.delta_latency
             }
        })

        val oldIsList = oldIs.toList(intCompare)

        val maxPtr = ledger.state.log.indices().filterMap(idx => {
            val b = ledger.state.log[idx]
            if(t - b.time > ledger.params.delta_slack) {
                Some(idx)
            } else {
                None
            }
        }).union(Set(ptr_prim)).setMax(0)

        (oldIsList.concat(newIs), maxPtr) 
    }
     
    pure def led_valid(vallidator: ValidatorState, block: Block): {val_state: ValidatorState, valid: bool} = {
        {val_state: vallidator, valid: true} // TODO
    }
        

    
    
}