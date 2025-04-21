# Tokenized Loyalty Points System
 
# Tokenized Loyalty Points System

A Clarity smart contract for managing a loyalty points system on the Stacks blockchain. This contract allows merchants to award points to customers for purchases, and customers can redeem those points for rewards.

## Features

- Merchant registration and management
- Points issuance based on purchase value
- Tiered loyalty system based on lifetime points
- Reward creation and redemption
- Tracking of customer engagement with merchants

## Contract Details

### Constants and Variables

- `points-per-stx`: Determines how many loyalty points are awarded per 1000 STX of purchase value
- `min-purchase-amount`: Minimum purchase amount required to earn points
- `redemption-rate`: Rate at which points can be redeemed for rewards
- `total-points-issued`: Total number of points issued across the system
- `total-points-redeemed`: Total number of points redeemed across the system

### User Tiers

- Tier 0: 0-999 lifetime points
- Tier 1: 1,000-4,999 lifetime points
- Tier 2: 5,000-19,999 lifetime points
- Tier 3: 20,000+ lifetime points

## Usage

### For Contract Owner

1. Register merchants:
```
(contract-call? .loyalty-points register-merchant "Merchant Name" u2)
```

2. Update points parameters:
```
(contract-call? .loyalty-points update-points-parameters u10 u1000 u100)
```

3. Update merchant details:
```
(contract-call? .loyalty-points update-merchant 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM "New Name" u3 true)
```

### For Merchants

1. Award points to customers:
```
(contract-call? .loyalty-points award-points 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 u100 u5000)
```

2. Create rewards:
```
(contract-call? .loyalty-points create-reward u500 "Free Coffee" "Redeem for a free coffee at any location" u10000)
```

3. Deactivate a reward:
```
(contract-call? .loyalty-points deactivate-reward u1)
```

### For Customers

1. Check your points balance:
```
(contract-call? .loyalty-points get-user-points 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)
```

2. Redeem a reward:
```
(contract-call? .loyalty-points redeem-reward u1)
```

3. Check your tier:
```
(contract-call? .loyalty-points get-user-tier 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)
```

## Testing with Clarinet

You can test this contract using Clarinet. Here's a basic test flow:

1. Register a merchant
2. Award points to a user
3. Create a reward
4. Redeem the reward

## License

This project is open source and available under the MIT License.
```

Git commit message:
```
Implementation for Tokenized Loyalty Points System
```

GitHub Pull Request title:
```
Tokenized Loyalty Points System - MVP Implementation
```

GitHub Pull Request description:
```
This PR introduces a Minimum Viable Product (MVP) for a Tokenized Loyalty Points System on the Stacks blockchain.

Key features:
- Merchant registration and management
- Points issuance based on purchase value
- Tiered loyalty system (4 tiers based on lifetime points)
- Reward creation and redemption
- Tracking of customer engagement with merchants

The implementation includes:
- Core smart contract with all necessary functions
- Comprehensive README with usage instructions
- Support for multiple merchants and customizable reward parameters

This MVP provides the foundation for a full-featured loyalty points system that can be extended with additional features in future iterations.
