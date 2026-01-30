# G-Ledger: Global Ledger with Consensus

## Overview

G-Ledger (Global Ledger) provides a formal specification of a distributed ledger functionality. This module models an ideal functionality that enables parties to submit transactions, read committed transaction logs, compute agreed prefix length, select commitee, etc. It implements the core guarantees of a blockchain ledger: liveness (transactions are committed), consistency (honest parties see prefix-consistent views), validity (only valid transactions are committed), and persistence (committed transactions never disappear).

## Module Structure

### Core Files

- **[g_ledger_types.qnt](g_ledger_types.qnt)**: Type definitions and data structures
  - `LED_Tx`: Transaction type (Normal, Install, Update with hash/signature)
  - `LED_Log`: Ordered list of transactions with timestamps
  - `LED_Buffer`: Temporary transaction storage with entry IDs
  - `Committee`: List of committee members for consensus
  - `LED_System`: System state with buffer, log, pointers per party, validation, committee selection, APL state

- **[g_ledger.qnt](g_ledger.qnt)**: Core ledger protocol 
  - Submit: Four-phase transaction submission with time/SIM coordination
  - Read: Complex transaction filtering and delivery with consistency guarantees
  - OK: Time advancement synchronization with G-Clock
  - Simulator control: Manages commit/abort decisions

- **[g_ledger_comsel.qnt](g_ledger_comsel.qnt)**: Committee Selection Protocol

- **[g_ledger_genesis.qnt](g_ledger_genesis.qnt)**: Genesis Block Protocol
  - Genesis: Initializes the ledger with a starting block

- **[g_ledger_apl.qnt](g_ledger_apl.qnt)**: Agreed Prefix Length
  - APL: Computes the agreed prefix length of the ledger

- **[g_ledger_ival.qnt](g_ledger_ival.qnt)**: Integrity and Validity Protocol
  - Eval: Validates transactions according to state machine semantics
  - IRead: Integrity-checked reads

- **[g_ledger_env.qnt](g_ledger_env.qnt)**: Environment
  - Request creation and response consumption

- **[g_ledger_properties.qnt](g_ledger_properties.qnt)**: Formal property specifications (324 lines)
- **Liveness**: Transaction submitted at time t appears in all party logs by time t + Δ_latency + Δ_slack
- **Consistency**: For any two parties P and Q, log_P is prefix of log_Q or vice versa
- **Validity**: All transactions in honest party's log satisfy validity predicate
- **Committee selection**: Committee members selected according to protocol rules
- **APL**: Agreed prefix length properties (same for everyone, only grows, etc.)
- **Integrity**: Ledger state remains consistent across all operations

## Functionality
![g_ledger](pic.png?raw=true)

