# F-Sig: Digital Signatures

## Overview

F-Sig (Digital Signatures functioanlity) provides a formal specification of an idealized digital signature scheme. This module models a cryptographic functionality that allows parties to generate verification keys, sign messages, and verify signatures. The functionality guarantees strong unforgeability.

## Module Structure

### Core Files

- **[f_sig_types.qnt](f_sig_types.qnt)**: Type definitions and data structures
  - `SIG_Message`: Union of all message types (GenRequest, SignRequest, VerifyRequest, responses, and SIM controls)
  - `SIG_SystemParams`: System parameters including signers S, parties P, corrupted parties C, and available keys/signatures
  - `SIG_StateVars`: Per-SID state containing VK table (mapping party to verification key) and Ver table (verification results)
  - `SIG_System`: Complete system state with parameters, per-SID state, input buffer, handles, nonce, and event log

- **[f_sig.qnt](f_sig.qnt)**: Core protocol implementation
  - Initialization: `sig_init`
  - Key Generation: Three-phase key generation with simulator control (`sig_gen_1`, `sig_gen_2`, `sig_gen_3`)
  - Sign: Three-phase signing operation with simulator control (`sig_sign_1`, `sig_sign_2`, `sig_sign_3`)
  - Verify: Three-phase verification with simulator control (`sig_verify_1`, `sig_verify_2`, `sig_verify_3`)
  - Simulator control: Manages the hold and release phases (`sig_sim_loses_ctrl`)



## Verified Properties [f_sig_properties.qnt](f_sig_properties.qnt)

- **One VK per User (OneVKperUser)**: Each party has at most one verification key per SID
- **VK Ownership Uniqueness (VKOwnershipUnique)**: Each verification key is owned by at most one party
- **Verification Consistency (VerConsistent)**: No contradictory verification results exist in the Ver table
- **Completeness (Completeness)**: If a message is signed with a verification key, verifying that signature with the same key succeeds
- **Strong Unforgeability (Unforgeability)**: A signature verifies for a message-key pair only if either the key owner is corrupted or the key owner previously signed that exact message with that signature


## Functionality

![f-sig](pic.png?raw=true)
