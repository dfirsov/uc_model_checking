# F-ATMS: Advanced Threshold Multi-Signature

## Overview

F-ATMS provides a formal specification of an advanced threshold multi-signature scheme. This module models an ideal cryptographic functionality that enables parties to generate individual keys, aggregate keys into threshold verification keys, and create both individual and aggregate signatures with full verification support.

## Module Structure

### Core Files

- **[f_atms_types.qnt](f_atms_types.qnt)**: Type definitions and data structures
  - `ATMS_VK`: Individual verification key (integer identifier)
  - `ATMS_AVK`: Aggregate verification key (integer identifier)
  - `ATMS_Document`: Message type with hash (h) and aggregate key (avk)
  - `ATMS_Signature`: Individual signature (integer)
  - `ATMS_ASignature`: Aggregate signature (integer)
  - `ATMS_System`: System state containing input buffer, handles, state per SID, parameters, nonce, and event log
  - State per SID: VK (keys), VER (individual verifications), VKs (key aggregations), AVK (aggregate keys), AVER (aggregate verifications), Signers (signing records), CVK (corrupted keys)

- **[f_atms.qnt](f_atms.qnt)**: Core protocol implementation 
  - Gen: Individual key generation (`atms_gen_1`, `atms_gen_2`, `atms_gen_3`)
  - AKey: Key aggregation (`atms_akey_1`, `atms_akey_2`, `atms_akey_3`)
  - ACheck: Aggregation validation (`atms_acheck_1`, `atms_acheck_2`, `atms_acheck_3`)
  - Sign: Individual signature creation (`atms_sign_1`, `atms_sign_2`, `atms_sign_3`)
  - Verify: Individual signature verification (`atms_verify_1`, `atms_verify_2`, `atms_verify_3`)
  - ASign: Aggregate signature creation (`atms_asign_1`, `atms_asign_2`, `atms_asign_3`)
  - AVerify: Aggregate signature verification (`atms_averify_1`, `atms_averify_2`, `atms_averify_3`)
  - Sanity checking: VK and AVK collision prevention

- **[f_atms_env.qnt](f_atms_env.qnt)**: 
  - Execution environment
  - Request creation and response consumption

- **[f_atms_properties.qnt](f_atms_properties.qnt)**: Formal property specifications 
  - Functionality and injectivity properties
  - Determinism properties
  - Correctness properties for signatures and aggregations
  - Unforgeability properties
  - Consistency properties
  - Handle management and input bounds

## Functionality
![f-atms](pic.png?raw=true)

