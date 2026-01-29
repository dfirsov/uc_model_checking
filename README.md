# UC and Model Checking in Quint

## Overview
This project implements formal verification of Universal Composability (UC) protocols using the Quint specification language. It provides a framework for modeling UC ideal functionalities and their properties.

## Quint Setup
For this project we used Quint 0.29.1 and Apalache 0.51.1

## Checking the specification
Quint provides different modes of checking the specification. We briefly explain some of them.
For more info, go to https://quint-lang.org/ 
### Simulation
In simulation mode the Quint will produce random traces and check the invariant after each "step".

```bash
quint run <spec_file> --invariant=<property> [--max-steps=n] [--max-samples=m]
```
### Verification with TLC
We can instruct Quint to compile its specification to TLA+ and use TLC to check the property. In this
mode TLC will try to exhaustively cover the entire model to generate a counterexample.

```bash
./check_with_tlc.sh <spec_file> [--invariant=<property>] [--temporal=<properties>]
```

### Verification with Apalache
In this mode the Quint spec will be compiled and checked with the Apalache SMT solver.

```bash
quint verify <spec_file> [--invariant=<property>] 
```

## Project Structure

### Specifications

#### Cryptographic Functionalities
- [**F-iNIZK**](specs/f_inizk/) - non-interactive ZK proofs for (i - interactive) relations
- [**F-ATMS**](specs/f_atms/) - Advanced threshold multi-signature scheme
- [**F-Hash**](specs/f_hash/) - Hash functionality with collision resistance
- [**F-PKI**](specs/f_pki/) - Public Key Infrastructure (registration, retrieval, verification)
- [**F-Diffuse**](specs/f_dif/) - Message diffuse protocol

#### Global Functionalities
- [**G-Clock**](specs/g_clock/) - Global clock for time synchronization
- [**G-Ledger**](specs/g_ledger/) - Global ledger with committee selection, APL, reads, and submits

#### Protocol
- [**Bridge**](specs/p_bridge/) - Cross-chain bridge protocol built from the above functionalities


## Key Features

### Modelling Approach
- **Choreographic modeling**: Explicit message passing between processes (parties, simulator, environment)
- **Local context transitions**: Each process defines reaction rules based on incoming messages
- **Custom effects**: Message exclusion and event logging for protocol control flow
- **Property checking**: Invariant verification via Apalache, TLC, and simulation

### Properties Verified

#### F-ATMS (Advanced Threshold Multi-Signature) - [f_atms_properties.qnt](specs/f_atms/f_atms_properties.qnt)
- **Functionality**: VK, VER, VKs, AVK, AVER are functional (deterministic mappings)
- **Injectivity**: VK, VKs, AVK are injective (unique keys identify parties/key sets)
- **Determinism**: Gen, ACheck, Verify, AVerify produce consistent outputs for same inputs
- **Non-bottom responses**: Sign returns valid signature when party has generated VK
- **Completeness**: Properly formed signatures successfully verify
- **Unforgeability**: Cannot verify aggregate signature without sufficient honest signatures
- **Handle management**: No orphaned simulation holds in the system



#### F-PKI (Public Key Infrastructure) - [f_pki_properties.qnt](specs/f_pki/f_pki_properties.qnt)
- **One entry per party**: Each party has at most one registration and verification entry per SID
- **Retrieve succeeds for registered**: Retrieval returns correct VK for registered parties
- **Retrieve fails for unregistered**: Retrieval returns None for unregistered parties
- **Certificate verification**: Cert verification succeeds iff party registered with matching (vk, cert) pair

#### F-Hash (Collision-Resistant Hash) - [f_hash_properties.qnt](specs/f_hash/f_hash_properties.qnt)
- **Correctness**: All parties get same hash for same input (P.Hash(x) = Q.Hash(x))
- **Collision resistance**: Different inputs produce different hashes (x ≠ y ⇒ Hash(x) ≠ Hash(y))

#### F-NIZK (Non-Interactive Zero-Knowledge) - [f_nizk_properties.qnt](specs/f_nizk/f_nizk_properties.qnt)
- **Prove soundness**: R(x,w) = 0 ⇒ Prove(x,w) = ⊥ (cannot prove false statements)
- **Verify soundness**: If no witness exists for x, then Verify(x,π) = false
- **Completeness**: R(x,w) = 1 ∧ Prove(x,w) = π ⇒ Verify(x,π) = true
- **Verify after prove**: Successful verification implies proof was previously generated
- **Proof non-malleability**: Same proof cannot verify different statements
- **Handle management**: Proper management of prove/verify simulation holds

#### F-iNIZK (NIZK with Interactive Rels) - [f_inizk_properties.qnt](specs/f_inizk/f_inizk_properties.qnt)
- **Prove soundness**: R(x,w) = 0 ⇒ Prove(x,w) = ⊥
- **Verify soundness**: ∀x: (∀w: R(x,w) = 0) ⇒ ∀Q,π: Verify(x,π) = 0
- **Completeness**: Valid witness guarantees successful proof and verification
- **Verify after prove**: Verification requires prior proof generation
- **Proof non-malleability**: Same proof verifies only one statement
- **Handle management**: Proper management of prove/verify/relation holds


#### F-Diffuse (Message Diffusion) - [f_dif_properties.qnt](specs/f_dif/f_dif_properties.qnt)
- **No duplication**: Message delivered at most once per party per time
- **Delta-delayed liveness**: Message diffused at time t is fetchable by time t + δ
- **Broadcast property**: If message diffused to multiple parties, eventually available to all

#### G-Clock (Global Clock) - [g_clock_properties.qnt](specs/g_clock/g_clock_properties.qnt)
- **Time monotonicity**: Clock time advances monotonically
- **Registration tracking**: Parties must register before using clock
- **Synchronization**: All registered parties see consistent time values
- **Handle management**: Proper management of clock simulation holds

#### G-Ledger (Global Ledger) - [g_ledger_properties.qnt](specs/g_ledger/g_ledger_properties.qnt)
- **Liveness**: Transaction submitted at time t appears in all party logs by time t + Δ_latency + Δ_slack
- **Consistency**: For any two parties P and Q, log_P is prefix of log_Q or vice versa
- **Validity**: All transactions in honest party's log satisfy validity predicate
- **Committee selection**: Committee members selected according to protocol rules
- **APL (Availability Proof Line)**: Availability proofs required for block inclusion
- **Integrity**: Ledger state remains consistent across all operations

#### P-Bridge (Cross-Chain Bridge Protocol) - [p_bridge_properties.qnt](specs/p_bridge/p_bridge_properties.qnt)
- **RoT agreement**: All parties agree on Rotation-of-Trust aggregate verification key
- **Bridge state consistency**: All parties agree on core bridge state (rot, cID, h, avk', aux, aux')
- **Aux update**: Auxiliary state updates correctly every R rounds (bst.aux @ (r+R) = bst.aux' @ r)
- **Bridge democracy**: Committee can change between rotation periods (may be violated)
- **State persistence**: Bridge state remains stable during non-rotation phases (within Δ+1 rounds)
- **Cross-chain message integrity**: Messages verified across chains maintain integrity
- **Key generation correctness**: Bridge validator keys generated and aggregated correctly
- **Rotation protocol safety**: Key rotation executes safely without disrupting ongoing operations






