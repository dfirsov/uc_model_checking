module g_ledger_tests {

    import basicSpells.* from "../../spells/basicSpells"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"
    import common.* from "../common"   

    import g_ledger_env.* from "./g_ledger_env" 


    run submit_cycle(party : Party, sid: SID, tx: TX) : bool = {
        (env_led_submit_reqs(party, sid, tx))
        .then(led_step)
        .then(gc_step_led_wait)
        .then(led_step)
        .then(led_gc_step_with(led_submit_3))
        .then(led_step)
        .then(env_consume_responses)        
    }

    run read_cycle(party : Party, sid: SID) : bool = {
        env_led_read_reqs(party, sid)
        .then(step)
        .then(led_read_2_act)
        .then(led_step)
        .then(gc_step_led_wait)
        .then(led_step)
        .then(env_consume_responses)      
    }

    run ok_cycle(party : Party, sid: SID) : bool = {
        env_ok_reqs(party, sid)
        .then(led_step)
        .then(gc_step_led_wait)
        .then(gc_step_led_wait)
        .then(gc_step_led_wait)
        .then(gc_step_led_wait)
        .then(led_step)
        .then(env_consume_responses)      
    }

    run test1 = {

        val party1 = 1
        val tx = 2
        val sid = 1

        init
        .then(ok_cycle(party1, sid))
        .then(ok_cycle(2,1))
        .expect( true ) // 
        .then(submit_cycle(party1, sid, tx))
        .expect( true ) // 
        .then(submit_cycle(2,sid,3))     
        .expect( true ) // 
        .then(read_cycle(party1, sid))
        
  
    }    
    
}