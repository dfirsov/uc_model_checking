# Model Checking UC Ideal Functionalities in Quint

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
quint run <spec_file> --invariant=<property> [--max-steps=n] [--max-samples=m] [--backend rust]
```

For example, to use simulator on all the specified properties of hash functions you can run the following code:
```bash
quint run specs/f_hash/f_hash_properties.qnt --invariant=AllProps --max-steps=500 --max-samples=10000 --backend rust
```
### Verification with TLC
We can instruct Quint to compile its specification to TLA+ and use TLC to check the property. In this
mode TLC will try to exhaustively cover the entire model to generate a counterexample.

```bash
./check_with_tlc.sh <spec_file> [--invariant <property>] [--temporal <properties>]
```
For example, to check with TLC all specified properties of F-PKI you can run the following code:
```bash
./check_with_tlc.sh specs/f_pki/f_pki_properties.qnt --invariant AllProps 
```

However, you might want to reduce the model size in [common.qnt](specs/common.qnt).

### Verification with Apalache
In this mode the Quint spec will be compiled and checked with the Apalache SMT solver.

```bash
quint verify <spec_file> [--invariant=<property>] 
```

For example, to verify all specified properties of Global clock with Apalache SMT solver you can run the following code:
```bash
quint verify specs/g_clock/g_clock_properties.qnt --invariant AllProps 
```

## Project Structure

### Specifications

Model size can be set in [common.qnt](specs/common.qnt).

#### Cryptographic Functionalities
- [**F-iNIZK**](specs/f_inizk/) - non-interactive ZK proofs for (i - interactive) relations
- [**F-NIZK**](specs/f_nizk/) - non-interactive ZK proofs for (pure) relations
- [**F-ATMS**](specs/f_atms/) - Advanced threshold multi-signature scheme
- [**F-Hash**](specs/f_hash/) - Hash functionality with collision resistance
- [**F-PKI**](specs/f_pki/) - Public Key Infrastructure (registration, retrieval, verification)
- [**F-Diffuse**](specs/f_dif/) - Message diffuse protocol
- [**F-Sig**](specs/f_sig/) - digital signature primitive
- [**F-AC**](specs/f_ac/) - authenticated channel
- [**F-CRS**](specs/f_crs/) - common reference string

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














