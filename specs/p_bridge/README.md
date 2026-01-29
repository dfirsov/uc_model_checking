# P-Bridge Protocol Specification

## Overview

The P-Bridge protocol is a comprehensive cross-chain bridge implementation that enables secure communication and asset transfer between two distributed ledgers. The protocol coordinates multiple distributed subsystems (G-Clock, F-PKI, F-ATMS, G-Ledger1, G-Ledger2, F-DIF, F-Hash, F-iNIZK) to achieve secure certification and verification of ledger states across multiple rounds.

## Module Structure

The P-Bridge protocol is implemented as follows:

- **p_bridge.qnt** : Core protocol framework and state management
- **p_bridge_types.qnt** : Message types, state variables, and system definitions
- **p_bridge_bkgen.qnt** : Background key generation protocol (4-phase) for cryptographic setup
- **p_bridge_rot.qnt** : Rotation of Trust (RoT) generation with committee formation
- **p_bridge_br.qnt** : bridge algorithm for ledger certification
- **p_bridge_led2.qnt**: Secondary ledger 
- **p_bridge_inizk.qnt**: Interactive NIZK proof integration
- **p_bridge_env.qnt**: Environment and adversary interaction layer
- **p_bridge_properties.qnt** : Formal properties and assertions

## Functionality
![p-bridge](pic1.png?raw=true)
![atms-algorithms](pic2.png?raw=true)

## Verified Properties [p_bridge_properties.qnt](p_bridge_properties.qnt)

- **RoT agreement**: All parties agree on Rotation-of-Trust aggregate verification key
- **Bridge state consistency**: All parties agree on core bridge state (rot, cID, h, avk', aux, aux')
- **Aux update**: Auxiliary state updates correctly every R rounds (bst.aux @ (r+R) = bst.aux' @ r)
- **Bridge democracy**: Committee can change between rotation periods (may be violated)
- **State persistence**: Bridge state remains stable during non-rotation phases (within Δ+1 rounds)
- **Cross-chain message integrity**: Messages verified across chains maintain integrity
- **Key generation correctness**: Bridge validator keys generated and aggregated correctly
- **Rotation protocol safety**: Key rotation executes safely without disrupting ongoing operations