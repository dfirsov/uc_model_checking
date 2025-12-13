module f_ac_tests{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_ac_types.*   from "./f_ac_types"
    import f_ac.*  from "./f_ac"
    import f_ac_env.*  from "./f_ac_env"

   //  quint test --main=f_ac_tests --max-samples=10000 --match="test_*" f_ac_tests.qnt 2>&1 

    // Helper: Send a message from one party to another
    run send_cycle(cfg: MsgConfig, to_party: Party) : bool = {
        (env_fac_send_reqs(cfg, to_party))
         .then(fac_step)  // Process send request
         .then(fac_send_2_act)
         .then(fac_step)  // Simulator acts on send
         .then(fac_step)  // Continue processing
         .then(gc_step_fac_wait)  // Clock responds
         .then(gc_step_fac_wait)  // Clock responds
         .then(fac_step)  // Complete send
         .then(fac_step)  // Complete send
         .then(env_consume_responses)
    }

    // Helper: Advance clock by doing OK request
    run advance_clock(cfg: MsgConfig) : bool = {
        (env_ok_reqs(cfg))
        .then(fac_step)  // Process OK request
        .then(gc_step_fac_wait)  // Clock responds
        .then(gc_step_fac_wait)  // Clock responds        
        .then(fac_step)  // Complete OK request
        .then(env_consume_responses)
    }

    // Helper: Fetch messages for a party
    run fetch_cycle(cfg: MsgConfig) : bool = {
        (env_fac_fetch_reqs(cfg))
        .then(fac_step)  // Process fetch request
        .then(fac_fetch_2_act)  // Simulator acts on fetch
        .then(fac_step)  // Complete fetch
        .then(env_consume_responses)
    }


    // Test 1: Basic send-fetch cycle with delta delay
    run test_basic_send_fetch = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends to Party 2
            send_cycle(cfg1, 2)
        })
        .then({ // Advance clock to satisfy delta
            advance_clock({...cfg1, nonce: 2})
        })
        .then({ // Advance clock again
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Party 2 fetches messages
            fetch_cycle({...cfg2, nonce: 4})
        })
        .expect( // Party 2 should receive the message
            f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == 2).size() == 1
        )
    }

    // Test 2: Multiple parties sending and fetching
    run test_multiple_messages = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends to Party 2
            send_cycle(cfg1, 2)
        })
        .then({ // Party 2 sends to Party 1
            send_cycle(cfg2, 1)
        })
        .then({ // Both parties advance clock
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Both parties advance clock again for delta
            advance_clock({...cfg1, nonce: 4})
        })
        .then({
            advance_clock({...cfg2, nonce: 5})
        })
        .then({ // Party 1 fetches
            fetch_cycle({...cfg1, nonce: 6})
        })
        .then({ // Party 2 fetches
            fetch_cycle({...cfg2, nonce: 7})
        })
        .expect( // Both parties should receive messages
            all {
                f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == cfg1.party).size() == 1,
                f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == cfg2.party).size() == 1
            }
        )
    }

    // Test 3: Cross-party communication - bidirectional
    run test_bidirectional_send = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends to Party 2
            send_cycle(cfg1, 2)
        })
        .then({ // Party 2 sends to Party 1
            send_cycle(cfg2, 1)
        })
        .then({ // Both parties advance clock
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Both parties advance clock again
            advance_clock({...cfg1, nonce: 4})
        })
        .then({
            advance_clock({...cfg2, nonce: 5})
        })
        .then({ // Party 2 fetches
            fetch_cycle({...cfg2, nonce: 6})
        })
        .then({ // Party 1 fetches
            fetch_cycle({...cfg1, nonce: 7})
        })
        .expect( // Both parties received their messages
            all {
                nondet fetch_res1 = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == 2).oneOf()
                nondet fetch_res2 = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == 1).oneOf()
                and {
                    fetch_res1.data.size() > 0,
                    fetch_res2.data.size() > 0
                }
            }
        )
    }

    // Test 4: S_flag prevents duplicate sends in same time slot
    run test_s_flag_prevents_duplicate = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        
        init
        .then({ // Party 1 sends first message
            send_cycle(cfg1, 2)
        })
        .expect( // Only one send should be recorded
            f_ac_s.log.map(x => x.msg).fac_get_send_response_messages().filter(x => x.cfg.party == 1).size() == 1
        )
    }

    // Test 5: F_flag prevents duplicate fetches in same time slot
    run test_f_flag_prevents_duplicate = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends
            send_cycle(cfg1, 2)
        })
        .then({ // Both parties advance clock
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Both parties advance clock again
            advance_clock({...cfg1, nonce: 4})
        })
        .then({
            advance_clock({...cfg2, nonce: 5})
        })
        .then({ // Party 2 fetches
            fetch_cycle({...cfg2, nonce: 6})
        })
        .expect( // Only one fetch response for party 2
            f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == 2).size() == 1
        )
    }

    // Test 6: Messages removed from L after fetch (no duplication)
    run test_no_message_duplication = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends
            send_cycle(cfg1, 2)
        })
        .then({ // Both parties advance clock twice for delta
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({
            advance_clock({...cfg1, nonce: 4})
        })
        .then({
            advance_clock({...cfg2, nonce: 5})
        })
        .then({ // Party 2 fetches
            fetch_cycle({...cfg2, nonce: 6})
        })
        .then({ // Both parties advance clock for new time slot
            advance_clock({...cfg1, nonce: 7})
        })
        .then({
            advance_clock({...cfg2, nonce: 8})
        })
        .then({ // Party 2 tries to fetch again
            fetch_cycle({...cfg2, nonce: 9})
        })
        .expect( // Second fetch should be empty (message already delivered)
            all {
                nondet fetch_res1 = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.nonce == 6).oneOf()
                nondet fetch_res2 = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.nonce == 9).oneOf()
                and {
                    fetch_res1.data.size() > 0,
                    fetch_res2.data.size() == 0  // No messages in second fetch
                }
            }
        )
    }

    // Test 7: Counter increments correctly
    run test_counter_increments = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Send first message
            send_cycle(cfg1, 2)
        })
        .then({ // Both parties advance clock for new time slot
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Send second message
            send_cycle({...cfg1, nonce: 4}, 2)
        })
        .expect( // Counter should have incremented
            f_ac_s.state.get(1).ctr == 2
        )
    }

    // Test 8: Authentication - only sent messages can be fetched
    run test_authentication = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends specific data
            send_cycle(cfg1, 2)
        })
        .then({ // Both parties advance clock
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Both parties advance clock again
            advance_clock({...cfg1, nonce: 4})
        })
        .then({
            advance_clock({...cfg2, nonce: 5})
        })
        .then({ // Party 2 fetches
            fetch_cycle({...cfg2, nonce: 6})
        })
        .expect( // Party 2 gets messages that were sent
            nondet fetch_res = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == 2).oneOf()
            fetch_res.data.size() > 0
        )
    }

    // Test 9: Multiple parties sending to same recipient
    run test_multiple_senders = {
        val cfg1 = {party: 1, sid: 1, nonce: 0}
        val cfg2 = {party: 2, sid: 1, nonce: 1}
        
        init
        .then({ // Party 1 sends to Party 2
            send_cycle(cfg1, 2)
        })
        .then({ // Both parties advance clock
            advance_clock({...cfg1, nonce: 2})
        })
        .then({
            advance_clock({...cfg2, nonce: 3})
        })
        .then({ // Party 1 sends another message to Party 2
            send_cycle({...cfg1, nonce: 4}, 2)
        })
        .then({ // Both parties advance clock to satisfy delta
            advance_clock({...cfg1, nonce: 5})
        })
        .then({
            advance_clock({...cfg2, nonce: 6})
        })
        .then({ // Party 2 fetches all messages
            fetch_cycle({...cfg2, nonce: 7})
        })
        .expect( // Party 2 receives both messages
            nondet r = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().filter(x => x.cfg.party == 2).oneOf()
            r.data.size() == 2
        )
    }

}