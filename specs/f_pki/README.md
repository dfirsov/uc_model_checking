# F-PKI: Public Key Infrastructure

## Overview

F-PKI (Functional Public Key Infrastructure) provides a formal specification of a public key infrastructure that supports registration, retrieval, and certificate verification of public keys. This module models an ideal functionality that allows parties to register their verification keys, enables other parties to retrieve keys, and supports verification of certificates binding keys to identities.

## Module Structure

### Core Files

- **[f_pki_types.qnt](f_pki_types.qnt)**: Type definitions and data structures
  - `VK`: Verification key type (integer identifier)
  - `Certificate`: Certificate type (integer identifier)
  - `PKI_Message`: Union of all message types (RegisterRequest, RetrieveRequest, CertVerifyRequest, GetAllRequest, responses, SIM controls)
  - `PKI_System`: System state containing input buffer, handles, state per SID, parameters, nonce, and event log
  - State per SID: Registration table `Reg`, Verification/Certificate table `Ver`

- **[f_pki.qnt](f_pki.qnt)**: Core protocol implementation
  - Initialization: `pki_init`
  - Register: Three-phase registration with simulator control (`pki_register_1`, `pki_register_2`, `pki_register_3`)
  - Retrieve: Three-phase retrieval with simulator control (`pki_retrieve_1`, `pki_retrieve_2`, `pki_retrieve_3`)
  - CertVerify: Certificate verification (`pki_certverify`)
  - GetAll: Retrieve all registered keys for an SID (`pki_get_all_1`, `pki_get_all_2`)

- **[f_pki_properties.qnt](f_pki_properties.qnt)**: Formal property specifications
  - One-entry-per-party invariants
  - Retrieve correctness properties
  - Certificate verification properties
  - Handle management and bounds checking

## Functionality

![f-pki](pic.png?raw=true)

## Verified Properties [f_pki_properties.qnt](f_pki_properties.qnt)

- **One entry per party**: Each party has at most one registration and verification entry per SID
- **Retrieve succeeds for registered**: Retrieval returns correct VK for registered parties
- **Retrieve fails for unregistered**: Retrieval returns None for unregistered parties
- **Certificate verification**: Cert verification succeeds iff party registered with matching (vk, cert) pair
