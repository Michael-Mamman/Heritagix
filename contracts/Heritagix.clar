(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))

(define-data-var next-heritage-id uint u1)
(define-data-var registration-fee uint u1000000)
(define-data-var verification-reward uint u500000)

(define-map heritage-registry
  uint
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 100),
    category: (string-ascii 50),
    submitter: principal,
    verifier: (optional principal),
    verified: bool,
    timestamp: uint,
    cultural-significance: uint,
    preservation-status: (string-ascii 20)
  }
)

(define-map heritage-by-location
  (string-ascii 100)
  (list 50 uint)
)

(define-map heritage-by-category
  (string-ascii 50)
  (list 100 uint)
)

(define-map user-submissions
  principal
  (list 20 uint)
)

(define-map user-verifications
  principal
  (list 20 uint)
)

(define-map heritage-votes
  uint
  {
    upvotes: uint,
    downvotes: uint,
    voters: (list 100 principal)
  }
)

(define-map user-reputation
  principal
  {
    submissions: uint,
    verifications: uint,
    reputation-score: uint
  }
)

(define-public (register-heritage (title (string-ascii 100)) (description (string-ascii 500)) (location (string-ascii 100)) (category (string-ascii 50)) (cultural-significance uint))
  (let
    (
      (heritage-id (var-get next-heritage-id))
      (fee (var-get registration-fee))
    )
    (asserts! (> (len title) u0) ERR_INVALID_INPUT)
    (asserts! (> (len description) u0) ERR_INVALID_INPUT)
    (asserts! (> (len location) u0) ERR_INVALID_INPUT)
    (asserts! (> (len category) u0) ERR_INVALID_INPUT)
    (asserts! (<= cultural-significance u10) ERR_INVALID_INPUT)
    (asserts! (>= (stx-get-balance tx-sender) fee) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? fee tx-sender CONTRACT_OWNER))
    
    (map-set heritage-registry heritage-id
      {
        title: title,
        description: description,
        location: location,
        category: category,
        submitter: tx-sender,
        verifier: none,
        verified: false,
        timestamp: stacks-block-height,
        cultural-significance: cultural-significance,
        preservation-status: "pending"
      }
    )
    
    (map-set heritage-by-location location
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? heritage-by-location location)) heritage-id) u50))
    )
    
    (map-set heritage-by-category category
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? heritage-by-category category)) heritage-id) u100))
    )
    
    (map-set user-submissions tx-sender
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? user-submissions tx-sender)) heritage-id) u20))
    )
    
    (map-set heritage-votes heritage-id
      {
        upvotes: u0,
        downvotes: u0,
        voters: (list)
      }
    )
    
    (update-user-reputation tx-sender "submission")
    (var-set next-heritage-id (+ heritage-id u1))
    (ok heritage-id)
  )
)

(define-public (verify-heritage (heritage-id uint))
  (let
    (
      (heritage (unwrap! (map-get? heritage-registry heritage-id) ERR_NOT_FOUND))
      (reward (var-get verification-reward))
    )
    (asserts! (not (is-eq tx-sender (get submitter heritage))) ERR_UNAUTHORIZED)
    (asserts! (not (get verified heritage)) ERR_ALREADY_EXISTS)
    
    (map-set heritage-registry heritage-id
      (merge heritage {
        verifier: (some tx-sender),
        verified: true,
        preservation-status: "verified"
      })
    )
    
    (map-set user-verifications tx-sender
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? user-verifications tx-sender)) heritage-id) u20))
    )
    
    (try! (as-contract (stx-transfer? reward tx-sender tx-sender)))
    (update-user-reputation tx-sender "verification")
    (ok true)
  )
)

(define-public (vote-heritage (heritage-id uint) (vote-type bool))
  (let
    (
      (heritage (unwrap! (map-get? heritage-registry heritage-id) ERR_NOT_FOUND))
      (current-votes (default-to {upvotes: u0, downvotes: u0, voters: (list)} (map-get? heritage-votes heritage-id)))
      (voters (get voters current-votes))
    )
    (asserts! (is-none (index-of voters tx-sender)) ERR_ALREADY_EXISTS)
    
    (map-set heritage-votes heritage-id
      {
        upvotes: (if vote-type (+ (get upvotes current-votes) u1) (get upvotes current-votes)),
        downvotes: (if vote-type (get downvotes current-votes) (+ (get downvotes current-votes) u1)),
        voters: (unwrap-panic (as-max-len? (append voters tx-sender) u100))
      }
    )
    (ok true)
  )
)

(define-public (update-preservation-status (heritage-id uint) (new-status (string-ascii 20)))
  (let
    (
      (heritage (unwrap! (map-get? heritage-registry heritage-id) ERR_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get submitter heritage)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    
    (map-set heritage-registry heritage-id
      (merge heritage {preservation-status: new-status})
    )
    (ok true)
  )
)

(define-private (update-user-reputation (user principal) (action (string-ascii 20)))
  (let
    (
      (current-rep (default-to {submissions: u0, verifications: u0, reputation-score: u0} (map-get? user-reputation user)))
    )
    (if (is-eq action "submission")
      (map-set user-reputation user
        {
          submissions: (+ (get submissions current-rep) u1),
          verifications: (get verifications current-rep),
          reputation-score: (+ (get reputation-score current-rep) u10)
        }
      )
      (map-set user-reputation user
        {
          submissions: (get submissions current-rep),
          verifications: (+ (get verifications current-rep) u1),
          reputation-score: (+ (get reputation-score current-rep) u25)
        }
      )
    )
  )
)

(define-public (set-registration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set registration-fee new-fee)
    (ok true)
  )
)

(define-public (set-verification-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set verification-reward new-reward)
    (ok true)
  )
)

(define-read-only (get-heritage (heritage-id uint))
  (map-get? heritage-registry heritage-id)
)

(define-read-only (get-heritage-by-location (location (string-ascii 100)))
  (map-get? heritage-by-location location)
)

(define-read-only (get-heritage-by-category (category (string-ascii 50)))
  (map-get? heritage-by-category category)
)

(define-read-only (get-user-submissions (user principal))
  (map-get? user-submissions user)
)

(define-read-only (get-user-verifications (user principal))
  (map-get? user-verifications user)
)

(define-read-only (get-heritage-votes (heritage-id uint))
  (map-get? heritage-votes heritage-id)
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation user)
)

(define-read-only (get-registration-fee)
  (var-get registration-fee)
)

(define-read-only (get-verification-reward)
  (var-get verification-reward)
)

(define-read-only (get-next-heritage-id)
  (var-get next-heritage-id)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)