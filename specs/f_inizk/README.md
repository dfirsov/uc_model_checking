# F-iNIZK: Non-Interactive Zero-Knowledge Proofs for (interactive) Relations

## Overview
F-iNIZK provides a formal specification of non-interactive zero-knowledge proofs with support for interactive relations. This module models a UC ideal functionality that allows parties to prove knowledge of witnesses for statements without revealing the witnesses.

## Module Structure
 
### Core Files

- **[f_inizk_types.qnt](f_inizk_types.qnt)**: Type definitions and data structures
  - `NIZK_Message`: Union of all message types (ProveRequest, VerifyRequest, RelRequest, ProveResponse, VerifyResponse, RelResponse, SIMControl, etc.)
  - `NIZK_System`: System state containing input buffer, handles, state per SID, parameters, nonce, and event log
  - State per SID: Verification table `Ver`, relation storage

- **[f_inizk.qnt](f_inizk.qnt)**: Core protocol implementation
  - Initialization: `nizk_init`
  - Transitions: Prove operations, Verify operations, Relation handling

- **[f_inizk_env.qnt](f_inizk_env.qnt)**: 
  - Request creation and response consumption

- **[f_inizk_properties.qnt](f_inizk_properties.qnt)**: Formal property specifications
  - Soundness properties
  - Completeness and consistency properties
  - Handle management and bounds checking

## Functionality

![f-sig](pic.png?raw=true)

## Verified Properties

- **Prove soundness**: If relation R(x,w) = 0 (witness doesn't satisfy statement), then Prove(x,w) returns ⊥
- **Verify soundness**: If no witness exists for statement x, then Verify(x,π) returns false for any proof π
- **Completeness**: If Prove(x,w) produces proof π when R(x,w) = 1, then Verify(x,π) returns true
- **Verify consistency**: If Ver[x,π] = true, then there exists witness w with R(x,w) = 1
- **No result flip-flop**: Verification results for (x,π) are stable (no contradictory entries)
- **Proof non-malleability**: Same proof π cannot verify different statements (π verifies unique x)
- **Verify after prove**: Successful verification implies proof was generated before (temporal causality)
- **Handle management**: Proper lifecycle management of prove/verify/relation simulation holds
- **Input bounds**: At most one message in input buffer at any time


