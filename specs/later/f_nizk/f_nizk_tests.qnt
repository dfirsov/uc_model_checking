module f_nizk_tests{
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"   
    import f_nizk_types.* from "./f_nizk_types"
    import f_nizk.* from "./f_nizk"
    import f_nizk_env.* from "./f_nizk_env" 


    // Helper cycle for prove operation (valid relation - goes through simulator)
    run prove_cycle(cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness) : bool = {
        (env_prove_reqs_args(cfg, x, w))
        .then(nizk_step)
        .then(nizk_prove_2_act) // forces simulator to act on the handle
        .then(nizk_step)
        .then(env_consume_responses)        
    }    

    // Helper cycle for prove operation with invalid witness (doesn't go through simulator)
    run prove_cycle_invalid(cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness) : bool = {
        (env_prove_reqs_args(cfg, x, w))
        .then(nizk_step)  // Returns ProveResponse with None immediately
        .then(env_consume_responses)        
    }

    // Helper cycle for verify operation
    run verify_cycle(cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof) : bool = {
        (env_verify_reqs_args(cfg, x, pi))
        .then(nizk_step)
        .then(nizk_verify_2_act) // forces simulator to act on the handle
        .then(nizk_step)
        .then(env_consume_responses)        
    }

    // Test 1: Completeness - Valid proof should verify successfully
    run test_completeness = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // prove with valid (statement, witness) pair
            prove_cycle(cfg, "Statement 1", 1)
        })
        .then({ // verify the proof
            nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            verify_cycle({...cfg, nonce: 2}, "Statement 1", prove_res.pi.unwrap())
        })
        .expect( // expect verification to be true
            nondet verify_res = f_nizk_s.log.map(x => x.msg).nizk_get_verify_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_res.b == true
        )
    }

    // Test 2: Prove Soundness - Invalid witness should fail to prove
    run test_prove_soundness = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // prove with invalid (statement, witness) pair
            prove_cycle_invalid(cfg, "Statement 1", 99) // 99 is not a valid witness for Statement 1
        })
        .expect( // expect proof to be None
            nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            prove_res.pi == None
        )
    }

    // Test 3: Verify Soundness - Non-existent statement should fail verification
    run test_verify_soundness = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // verify a proof for a statement with no witness in relation
            verify_cycle(cfg, "NonExistentStatement", 5)
        })
        .expect( // expect verification to be false
            nondet verify_res = f_nizk_s.log.map(x => x.msg).nizk_get_verify_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            verify_res.b == false
        )
    }

    // Test 4: Multiple valid witnesses for same statement
    run test_multiple_witnesses = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // prove with first valid witness
            prove_cycle(cfg, "Statement 3", 3)
        })
        .then({ // prove with second valid witness for same statement
            prove_cycle({...cfg, nonce: 2}, "Statement 3", 7)
        })
        .expect( // both proofs should be generated (not None)
            all {
                nondet prove_res1 = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
                nondet prove_res2 = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                and {
                    prove_res1.pi != None,
                    prove_res2.pi != None
                }
            }
        )
    }

    // Test 5: Prove-Verify full cycle with valid statement-witness
    run test_prove_verify_cycle = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // prove 
            prove_cycle(cfg, "Statement 2", 2)
        })
        .then({ // verify the generated proof
            nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            verify_cycle({...cfg, nonce: 2}, prove_res.x, prove_res.pi.unwrap())
        })
        .expect(
            all {
                // Proof was generated successfully
                nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
                // Verification succeeded
                nondet verify_res = f_nizk_s.log.map(x => x.msg).nizk_get_verify_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                and {
                    prove_res.pi != None,
                    verify_res.b == true,
                    prove_res.x == verify_res.x
                }
            }
        )
    }

    // Test 6: Verify consistency - same proof for same statement should always give same result
    run test_verify_consistency = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // prove
            prove_cycle(cfg, "Statement 4", 4)
        })
        .then({ // verify first time
            nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            verify_cycle({...cfg, nonce: 2}, "Statement 4", prove_res.pi.unwrap())
        })
        .then({ // verify second time with same proof
            nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            verify_cycle({...cfg, nonce: 3}, "Statement 4", prove_res.pi.unwrap())
        })
        .expect( // both verifications should return the same result
            all {
                nondet verify_res1 = f_nizk_s.log.map(x => x.msg).nizk_get_verify_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                nondet verify_res2 = f_nizk_s.log.map(x => x.msg).nizk_get_verify_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
                verify_res1.b == verify_res2.b
            }
        )
    }

    // Test 7: Wrong witness for statement fails to prove
    run test_wrong_witness = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // try to prove Statement 1 with witness 2 (which is for Statement 2)
            prove_cycle_invalid(cfg, "Statement 1", 2)
        })
        .expect( // expect proof to be None
            nondet prove_res = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            prove_res.pi == None
        )
    }

    // Test 8: Multiple parties can prove and verify independently
    run test_multiple_parties = {
        val cfg1 = {party: 1, sid: 1, nonce: 1}
        val cfg2 = {party: 2, sid: 1, nonce: 2}
        
        init
        .then({ // party 1 proves
            prove_cycle(cfg1, "Statement 1", 1)
        })
        .then({ // party 2 proves different statement
            prove_cycle(cfg2, "Statement 2", 2)
        })
        .expect( // both proofs should be generated
            all {
                nondet prove_res1 = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
                nondet prove_res2 = f_nizk_s.log.map(x => x.msg).nizk_get_prove_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                and {
                    prove_res1.pi != None,
                    prove_res2.pi != None
                }
            }
        )
    }
}
