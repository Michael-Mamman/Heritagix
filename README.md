# 🏛️ Heritagix - Cultural Heritage Registry

> 🌍 Preserving local culture on the blockchain, one heritage site at a time

## 📖 Overview

Heritagix is a decentralized cultural heritage registry built on the Stacks blockchain. It enables communities worldwide to document, verify, and preserve their cultural heritage sites, traditions, and artifacts in an immutable, transparent manner.

## ✨ Features

- 📝 **Heritage Registration**: Submit cultural heritage sites with detailed descriptions
- ✅ **Community Verification**: Peer-to-peer verification system with rewards
- 🗳️ **Voting System**: Community-driven quality control through upvotes/downvotes
- 🏆 **Reputation System**: Build credibility through contributions and verifications
- 🗺️ **Location-based Discovery**: Find heritage sites by geographic location
- 🏷️ **Category Organization**: Browse heritage by cultural categories
- 💰 **Economic Incentives**: Registration fees and verification rewards

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
git clone <repository-url>
cd heritagix
clarinet check
```

## 🔧 Usage

### Register a Heritage Site

```clarity
(contract-call? .heritagix register-heritage 
  "Ancient Temple of Wisdom" 
  "A 500-year-old temple representing local architectural heritage"
  "Kyoto, Japan"
  "Architecture"
  u8)
```

### Verify a Heritage Site

```clarity
(contract-call? .heritagix verify-heritage u1)
```

### Vote on Heritage Quality

```clarity
(contract-call? .heritagix vote-heritage u1 true)
```

### Update Preservation Status

```clarity
(contract-call? .heritagix update-preservation-status u1 "restored")
```

## 📊 Read-Only Functions

### Get Heritage Information
```clarity
(contract-call? .heritagix get-heritage u1)
```

### Find Heritage by Location
```clarity
(contract-call? .heritagix get-heritage-by-location "Kyoto, Japan")
```

### Check User Reputation
```clarity
(contract-call? .heritagix get-user-reputation 'SP1234...)
```

## 💡 Key Concepts

### 🏛️ Heritage Registry
Each heritage entry contains:
- Title and detailed description
- Geographic location
- Cultural category
- Submitter and verifier information
- Community votes and verification status
- Cultural significance rating (1-10)
- Preservation status

### 🎯 Reputation System
Users earn reputation through:
- **Submissions**: +10 points per heritage registered
- **Verifications**: +25 points per heritage verified
- Quality contributions increase community standing

### 💰 Economic Model
- **Registration Fee**: 1 STX (configurable)
- **Verification Reward**: 0.5 STX (configurable)
- Fees fund the verification reward pool

## 🌟 Categories

Popular heritage categories include:
- 🏗️ Architecture
- 🎭 Traditions
- 🎨 Arts & Crafts
- 🍜 Culinary Heritage
- 🎵 Music & Dance
- 📚 Literature
- 🏞️ Natural Sites

## 🔒 Security Features

- Owner-only administrative functions
- Anti-spam through registration fees
- Self-verification prevention
- Duplicate vote protection
- Input validation and error handling

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Submit heritage entries for testing
4. Open a pull request

## 📄 License

MIT License - Preserving culture for everyone

## 🌐 Community

Join our mission to preserve global cultural heritage through blockchain technology!



