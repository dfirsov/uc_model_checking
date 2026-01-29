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

### Properties 











#### P-Bridge (Cross-Chain Bridge Protocol) - [p_bridge_properties.qnt](specs/p_bridge/p_bridge_properties.qnt)
- **RoT agreement**: All parties agree on Rotation-of-Trust aggregate verification key
- **Bridge state consistency**: All parties agree on core bridge state (rot, cID, h, avk', aux, aux')
- **Aux update**: Auxiliary state updates correctly every R rounds (bst.aux @ (r+R) = bst.aux' @ r)
- **Bridge democracy**: Committee can change between rotation periods (may be violated)
- **State persistence**: Bridge state remains stable during non-rotation phases (within Δ+1 rounds)
- **Cross-chain message integrity**: Messages verified across chains maintain integrity
- **Key generation correctness**: Bridge validator keys generated and aggregated correctly
- **Rotation protocol safety**: Key rotation executes safely without disrupting ongoing operations






