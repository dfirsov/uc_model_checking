# UC and Model Checking in Quint

## Overview
This project implements formal verification of Universal Composability (UC) protocols using the Quint specification language. It provides a framework for modeling UC ideal functionalities and their properties.

## Project Structure

### Specifications

#### Cryptographic Functionalities
- [**F-iNIZK**](specs/f_inick/) - non-interactive ZK proofs for interactive relations
- [**F-ATMS**](specs/f_atms/) - Aggregate threshold multi-signature scheme with correctness and unforgeability properties
- [**F-Hash**](specs/f_hash/) - Hash functionality with collision resistance
- [**F-PKI**](specs/f_pki/) - Public Key Infrastructure (registration, retrieval, verification)
- [**F-Diffuse**](specs/f_dif/) - Message diffuse protocol

#### Global Functionalities
- [**G-Clock**](specs/g_clock/) - Global clock for time synchronization
- [**G-Ledger**](specs/g_ledger/) - Global ledger with committee selection, APL, and interval validation

#### Protocol
- [**Bridge**](specs/p_bridge/) - Cross-chain bridge protocol combining the above functionalities


## Key Features

### Verification Approach
- **Choreographic modeling**: Explicit message passing between processes (parties, simulator, environment)
- **Local context transitions**: Each process defines reaction rules based on incoming messages
- **Custom effects**: Message exclusion and event logging for protocol control flow
- **Property checking**: Invariant verification via Apalache, TLC, and simulation

### Properties Verified

#### f_atms (Aggregate Threshold Multi-Signature)
- **Functionality**: VK, VER, VKs, AVK, AVER functional (deterministic)
- **Injectivity**: Keys uniquely identify parties
- **Correctness**: Properly formed signatures and threshold properties
- **Unforgeability**: Attacker cannot forge valid aggregate signatures

#### f_sig (Signature Scheme)
- **Key consistency**: Environment receives consistent verification keys
- **Verification soundness**: Verification results consistent with signing
- **Liveness**: Honest parties can always obtain keys and signatures

#### f_nizk (Zero-Knowledge)
- **No flip-flops**: Inconsistent verification results prevented
- **Consistency**: Verified statements have supporting witnesses
- **Soundness & Completeness**: Correct proof verification and valid completeness

#### g_ledger (Ledger)
- **Liveness**: Transaction delivery within latency + slack bounds
- **Consistency**: Agreed prefix property
- **Committee selection correctness**: Committee composition and security parameters

## Usage

### Running Model Checking
```bash
./check_with_tlc.sh <spec_file> [--invariant=<property>] [--temporal=<properties>] [--workers=<n>]
```


