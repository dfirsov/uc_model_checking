# F-Hash: Collision-Resistant Hash Function

## Overview

F-Hash provides a formal specification of an idealized collision-resistant hash function. This module models a cryptographic functionality that allows parties to hash arbitrary data and guarantees that all parties receive the same hash for identical inputs while maintaining collision resistance (different inputs produce different hashes).

## Module Structure

### Core Files

- **[f_hash_types.qnt](f_hash_types.qnt)**: Type definitions and data structures
  - `Hash_Input`: Input to hash function (list of transactions)
  - `Hash_Output`: Hash function output (integer identifier)
  - `HASH_Message`: Union of all message types (HashRequest, HashResponse, SIM controls)
  - `HASH_System`: System state containing input buffer, handles, state per SID, parameters, nonce, and event log
  - State per SID: Hash mapping `hash_map` from inputs to outputs

- **[f_hash.qnt](f_hash.qnt)**: Core protocol implementation
  - Initialization: `hash_init`
  - Three-phase hash operation with simulator control (`hash_hash_1`, `hash_hash_2`, `hash_hash_3`)
  - Sanity checking: `san_hash` validates hash candidates against collision resistance
  - Clean hash check: `clean_hash` ensures no collisions in the hash map
  - Simulator loss: `hash_sim_loses_ctrl` handles simulator state loss

- **[f_hash_properties.qnt](f_hash_properties.qnt)**: Formal property specifications
  - Correctness: All parties get same hash for same input
  - Collision resistance: Different inputs produce different hashes

## Functionality
![f-hash](pic.png?raw=true)

## Verified Properties [f_hash_properties.qnt](f_hash_properties.qnt)

- **Correctness**: All parties get same hash for same input (P.Hash(x) = Q.Hash(x))
- **Collision resistance**: Different inputs produce different hashes (x ≠ y ⇒ Hash(x) ≠ Hash(y))