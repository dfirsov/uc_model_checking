module g_ledger_env {
    
    import basicSpells.* from "../../spells/basicSpells"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"
    import common.* from "../common"    

   val ALL_TXS = 1.to(20)
   val Parties = Set(1,2)
   val C = Set()
   val sids = Set(1)
   val delta_latency = 0
   val delta_slack = 0
   //pure def Val : LED_ValidatorState = (parties, tx) => true


    action step : bool = any {
        led_step,  
        gc_step_led_wait,
        env_led_send_all_reqs,
        env_consume_responses,
    }

    action gc_step_led_wait : bool =  all {
        g_ledger_s' = g_ledger_s,
        gc_step,        
    }

    pure def env_consume_response(sys: LED_System) : Set[LED_Transition] = {
         sys.input.led_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                LED_LogMessage(msg_log),
                LED_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(g_ledger_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        g_ledger_s' = led_process_transitions(g_ledger_s, transition),
        gc_stutter
   }        
  
    action init = all {
        led_init(Parties, C, sids, delta_latency, delta_slack, true),
        gc_init(C,0,Set({party: 1, sid: 1}, {party:2, sid:1})),
    }

    action env_led_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()
        nondet tx = ALL_TXS.oneOf()
        any {
            env_led_submit_reqs(from, sid, tx), 
            env_led_read_reqs(from, sid),
            // bias toward ok requests
            env_ok_reqs(from, sid),
            env_ok_reqs(from, sid),
            //env_ok_reqs(from, sid),
            //env_ok_reqs(from, sid),
            env_led_sim_reqs,
            env_gc_sim_reqs,
        }
    }

 
    // Environment interaction: non-deterministically add requests when input is empty
    action env_led_submit_reqs(from:Party, sid: SID, tx: TX) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
        g_ledger_s' = {...g_ledger_s, 
            input: Set(SubmitRequest({cfg: cfg, tx: tx})),
            nonce: g_ledger_s.nonce + 1

        },
        gc_stutter
    }

    action env_led_read_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}

        // Option 2: Fetch
        g_ledger_s' = {...g_ledger_s, 
            input: Set(ReadRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1

        },
        gc_stutter   
    }

    action env_ok_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}

        // Option 3: OK
        g_ledger_s' = {...g_ledger_s, 
            input: Set(OKRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1

        },   
        gc_stutter     
    }

    action env_led_sim_reqs : bool = all {
        gc_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        g_ledger_s' = {...g_ledger_s, 
            input: Set(SIMControl),
            nonce: g_ledger_s.nonce + 1
        },
        gc_stutter
    }

    action env_gc_sim_reqs : bool = all {
        gc_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        gc_s' = {...gc_s, 
            input: Set(GC_SIMControl),
            nonce: gc_s.nonce + 1
        },
        g_ledger_s' = g_ledger_s,
    }
       
}