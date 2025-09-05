(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_MEDIA_NOT_FOUND (err u105))
(define-constant ERR_MEDIA_ALREADY_EXISTS (err u106))
(define-constant ERR_INVALID_MEDIA_TYPE (err u107))
(define-constant ERR_MEDIA_SIZE_EXCEEDED (err u108))
(define-constant ERR_ACCESS_DENIED (err u109))
(define-constant ERR_EVENT_NOT_FOUND (err u110))
(define-constant ERR_EVENT_ALREADY_EXISTS (err u111))
(define-constant ERR_INVALID_DATE (err u112))
(define-constant ERR_INVALID_EVENT_TYPE (err u113))
(define-constant ERR_FUND_NOT_FOUND (err u114))
(define-constant ERR_FUND_ALREADY_EXISTS (err u115))
(define-constant ERR_FUND_CLOSED (err u116))
(define-constant ERR_MILESTONE_NOT_FOUND (err u117))
(define-constant ERR_MILESTONE_COMPLETED (err u118))

(define-data-var next-heritage-id uint u1)
(define-data-var registration-fee uint u1000000)
(define-data-var verification-reward uint u500000)
(define-data-var next-media-id uint u1)
(define-data-var media-upload-fee uint u100000)
(define-data-var max-media-size uint u10485760)
(define-data-var next-event-id uint u1)
(define-data-var event-submission-fee uint u50000)
(define-data-var next-fund-id uint u1)
(define-data-var fund-creation-fee uint u500000)

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

(define-map media-registry
  uint
  {
    heritage-id: uint,
    filename: (string-ascii 100),
    media-type: (string-ascii 20),
    file-size: uint,
    file-hash: (string-ascii 64),
    uploader: principal,
    upload-timestamp: uint,
    access-level: (string-ascii 10),
    approved: bool,
    moderator: (optional principal),
    description: (string-ascii 200),
    tags: (list 10 (string-ascii 30))
  }
)

(define-map heritage-media-files
  uint
  (list 50 uint)
)

(define-map media-access-permissions
  uint
  {
    public-access: bool,
    restricted-users: (list 20 principal),
    moderator-only: bool
  }
)

(define-map media-by-type
  (string-ascii 20)
  (list 100 uint)
)

(define-map user-media-uploads
  principal
  (list 30 uint)
)

(define-map historical-events
  uint
  {
    heritage-id: uint,
    event-title: (string-ascii 100),
    event-description: (string-ascii 500),
    event-date: uint,
    event-type: (string-ascii 30),
    historical-period: (string-ascii 50),
    significance-level: uint,
    submitter: principal,
    verified: bool,
    verifier: (optional principal),
    submission-timestamp: uint,
    source-references: (list 5 (string-ascii 200)),
    related-events: (list 10 uint)
  }
)

(define-map heritage-timeline
  uint
  (list 100 uint)
)

(define-map events-by-period
  (string-ascii 50)
  (list 200 uint)
)

(define-map events-by-type
  (string-ascii 30)
  (list 150 uint)
)

(define-map user-event-submissions
  principal
  (list 50 uint)
)

(define-map event-verification-votes
  uint
  {
    support-votes: uint,
    dispute-votes: uint,
    voters: (list 50 principal),
    consensus-reached: bool
  }
)

(define-map chronological-index
  {heritage-id: uint, date-range: (string-ascii 20)}
  (list 50 uint)
)

;; Heritage Conservation Fund System
(define-map conservation-funds
  uint
  {
    heritage-id: uint,
    fund-title: (string-ascii 100),
    fund-description: (string-ascii 300),
    target-amount: uint,
    current-amount: uint,
    fund-creator: principal,
    created-at: uint,
    deadline: uint,
    status: (string-ascii 20), ;; "active", "completed", "expired"
    milestones-count: uint,
    current-milestone: uint
  }
)

(define-map fund-contributions
  {fund-id: uint, contributor: principal}
  {
    amount: uint,
    contributed-at: uint
  }
)

(define-map fund-milestones
  {fund-id: uint, milestone-id: uint}
  {
    milestone-title: (string-ascii 100),
    required-amount: uint,
    completed: bool,
    completion-date: (optional uint),
    evidence-hash: (optional (string-ascii 64))
  }
)

(define-map heritage-fund-list
  uint
  (list 10 uint)
)

(define-map user-contributions
  principal
  (list 50 {fund-id: uint, amount: uint})
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

(define-public (upload-media (heritage-id uint) (filename (string-ascii 100)) (media-type (string-ascii 20)) (file-size uint) (file-hash (string-ascii 64)) (access-level (string-ascii 10)) (description (string-ascii 200)) (tags (list 10 (string-ascii 30))))
  (let
    (
      (media-id (var-get next-media-id))
      (upload-fee (var-get media-upload-fee))
      (max-size (var-get max-media-size))
      (heritage (unwrap! (map-get? heritage-registry heritage-id) ERR_NOT_FOUND))
    )
    (asserts! (> (len filename) u0) ERR_INVALID_INPUT)
    (asserts! (> (len media-type) u0) ERR_INVALID_INPUT)
    (asserts! (> (len file-hash) u0) ERR_INVALID_INPUT)
    (asserts! (<= file-size max-size) ERR_MEDIA_SIZE_EXCEEDED)
    (asserts! (or (is-eq media-type "image") (is-eq media-type "video") (is-eq media-type "document") (is-eq media-type "audio")) ERR_INVALID_MEDIA_TYPE)
    (asserts! (or (is-eq access-level "public") (is-eq access-level "private") (is-eq access-level "restricted")) ERR_INVALID_INPUT)
    (asserts! (>= (stx-get-balance tx-sender) upload-fee) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-none (map-get? media-registry media-id)) ERR_MEDIA_ALREADY_EXISTS)
    
    (try! (stx-transfer? upload-fee tx-sender CONTRACT_OWNER))
    
    (map-set media-registry media-id
      {
        heritage-id: heritage-id,
        filename: filename,
        media-type: media-type,
        file-size: file-size,
        file-hash: file-hash,
        uploader: tx-sender,
        upload-timestamp: stacks-block-height,
        access-level: access-level,
        approved: false,
        moderator: none,
        description: description,
        tags: tags
      }
    )
    
    (map-set heritage-media-files heritage-id
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? heritage-media-files heritage-id)) media-id) u50))
    )
    
    (map-set media-access-permissions media-id
      {
        public-access: (is-eq access-level "public"),
        restricted-users: (list),
        moderator-only: (is-eq access-level "private")
      }
    )
    
    (map-set media-by-type media-type
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? media-by-type media-type)) media-id) u100))
    )
    
    (map-set user-media-uploads tx-sender
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? user-media-uploads tx-sender)) media-id) u30))
    )
    
    (var-set next-media-id (+ media-id u1))
    (ok media-id)
  )
)

(define-public (approve-media (media-id uint))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
      (heritage (unwrap! (map-get? heritage-registry (get heritage-id media)) ERR_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get submitter heritage))) ERR_UNAUTHORIZED)
    
    (map-set media-registry media-id
      (merge media {
        approved: true,
        moderator: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (reject-media (media-id uint))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
      (heritage (unwrap! (map-get? heritage-registry (get heritage-id media)) ERR_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get submitter heritage))) ERR_UNAUTHORIZED)
    
    (map-delete media-registry media-id)
    (ok true)
  )
)

(define-public (update-media-access (media-id uint) (new-access-level (string-ascii 10)))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
      (heritage (unwrap! (map-get? heritage-registry (get heritage-id media)) ERR_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get uploader media)) (is-eq tx-sender (get submitter heritage)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq new-access-level "public") (is-eq new-access-level "private") (is-eq new-access-level "restricted")) ERR_INVALID_INPUT)
    
    (map-set media-registry media-id
      (merge media {access-level: new-access-level})
    )
    
    (map-set media-access-permissions media-id
      {
        public-access: (is-eq new-access-level "public"),
        restricted-users: (get restricted-users (default-to {public-access: false, restricted-users: (list), moderator-only: false} (map-get? media-access-permissions media-id))),
        moderator-only: (is-eq new-access-level "private")
      }
    )
    (ok true)
  )
)

(define-public (grant-media-access (media-id uint) (user principal))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
      (heritage (unwrap! (map-get? heritage-registry (get heritage-id media)) ERR_NOT_FOUND))
      (current-permissions (default-to {public-access: false, restricted-users: (list), moderator-only: false} (map-get? media-access-permissions media-id)))
    )
    (asserts! (or (is-eq tx-sender (get uploader media)) (is-eq tx-sender (get submitter heritage)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get access-level media) "restricted") ERR_INVALID_INPUT)
    
    (map-set media-access-permissions media-id
      (merge current-permissions {
        restricted-users: (unwrap-panic (as-max-len? (append (get restricted-users current-permissions) user) u20))
      })
    )
    (ok true)
  )
)

(define-public (revoke-media-access (media-id uint) (user principal))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
      (heritage (unwrap! (map-get? heritage-registry (get heritage-id media)) ERR_NOT_FOUND))
      (current-permissions (default-to {public-access: false, restricted-users: (list), moderator-only: false} (map-get? media-access-permissions media-id)))
      (filtered-users (get result (remove-user-from-list user (get restricted-users current-permissions))))
    )
    (asserts! (or (is-eq tx-sender (get uploader media)) (is-eq tx-sender (get submitter heritage)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    
    (map-set media-access-permissions media-id
      (merge current-permissions {
        restricted-users: filtered-users
      })
    )
    (ok true)
  )
)

(define-public (delete-media (media-id uint))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get uploader media)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    
    (map-delete media-registry media-id)
    (map-delete media-access-permissions media-id)
    (ok true)
  )
)

(define-public (set-media-upload-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set media-upload-fee new-fee)
    (ok true)
  )
)

(define-public (set-max-media-size (new-size uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set max-media-size new-size)
    (ok true)
  )
)

(define-private (remove-user-from-list (user-to-remove principal) (users-list (list 20 principal)))
  (fold remove-user-helper users-list {target: user-to-remove, result: (list)})
)

(define-private (remove-user-helper (user principal) (acc {target: principal, result: (list 20 principal)}))
  (if (is-eq user (get target acc))
    acc
    (merge acc {result: (unwrap-panic (as-max-len? (append (get result acc) user) u20))})
  )
)

(define-private (check-media-access (media-id uint) (user principal))
  (let
    (
      (media (unwrap! (map-get? media-registry media-id) ERR_MEDIA_NOT_FOUND))
      (permissions (default-to {public-access: false, restricted-users: (list), moderator-only: false} (map-get? media-access-permissions media-id)))
    )
    (ok (or 
      (get public-access permissions)
      (is-eq user (get uploader media))
      (is-eq user CONTRACT_OWNER)
      (is-some (index-of (get restricted-users permissions) user))
    ))
  )
)

(define-read-only (get-media (media-id uint))
  (map-get? media-registry media-id)
)

(define-read-only (get-heritage-media (heritage-id uint))
  (map-get? heritage-media-files heritage-id)
)

(define-read-only (get-media-access-permissions (media-id uint))
  (map-get? media-access-permissions media-id)
)

(define-read-only (get-media-by-type (media-type (string-ascii 20)))
  (map-get? media-by-type media-type)
)

(define-read-only (get-user-media-uploads (user principal))
  (map-get? user-media-uploads user)
)

(define-read-only (get-media-upload-fee)
  (var-get media-upload-fee)
)

(define-read-only (get-max-media-size)
  (var-get max-media-size)
)

(define-read-only (get-next-media-id)
  (var-get next-media-id)
)

(define-read-only (can-access-media (media-id uint) (user principal))
  (check-media-access media-id user)
)

(define-public (submit-historical-event (heritage-id uint) (event-title (string-ascii 100)) (event-description (string-ascii 500)) (event-date uint) (event-type (string-ascii 30)) (historical-period (string-ascii 50)) (significance-level uint) (source-references (list 5 (string-ascii 200))) (related-events (list 10 uint)))
  (let
    (
      (event-id (var-get next-event-id))
      (submission-fee (var-get event-submission-fee))
      (heritage (unwrap! (map-get? heritage-registry heritage-id) ERR_NOT_FOUND))
    )
    (asserts! (> (len event-title) u0) ERR_INVALID_INPUT)
    (asserts! (> (len event-description) u0) ERR_INVALID_INPUT)
    (asserts! (> (len event-type) u0) ERR_INVALID_INPUT)
    (asserts! (> (len historical-period) u0) ERR_INVALID_INPUT)
    (asserts! (<= significance-level u10) ERR_INVALID_INPUT)
    (asserts! (> event-date u0) ERR_INVALID_DATE)
    (asserts! (<= event-date stacks-block-height) ERR_INVALID_DATE)
    (asserts! (or (is-eq event-type "construction") (is-eq event-type "destruction") (is-eq event-type "renovation") (is-eq event-type "discovery") (is-eq event-type "cultural") (is-eq event-type "political") (is-eq event-type "natural") (is-eq event-type "archaeological")) ERR_INVALID_EVENT_TYPE)
    (asserts! (>= (stx-get-balance tx-sender) submission-fee) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? submission-fee tx-sender CONTRACT_OWNER))
    
    (map-set historical-events event-id
      {
        heritage-id: heritage-id,
        event-title: event-title,
        event-description: event-description,
        event-date: event-date,
        event-type: event-type,
        historical-period: historical-period,
        significance-level: significance-level,
        submitter: tx-sender,
        verified: false,
        verifier: none,
        submission-timestamp: stacks-block-height,
        source-references: source-references,
        related-events: related-events
      }
    )
    
    (map-set heritage-timeline heritage-id
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? heritage-timeline heritage-id)) event-id) u100))
    )
    
    (map-set events-by-period historical-period
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? events-by-period historical-period)) event-id) u200))
    )
    
    (map-set events-by-type event-type
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? events-by-type event-type)) event-id) u150))
    )
    
    (map-set user-event-submissions tx-sender
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? user-event-submissions tx-sender)) event-id) u50))
    )
    
    (map-set event-verification-votes event-id
      {
        support-votes: u0,
        dispute-votes: u0,
        voters: (list),
        consensus-reached: false
      }
    )
    
    (update-chronological-index heritage-id event-date event-id)
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

(define-public (verify-historical-event (event-id uint))
  (let
    (
      (event (unwrap! (map-get? historical-events event-id) ERR_EVENT_NOT_FOUND))
      (heritage (unwrap! (map-get? heritage-registry (get heritage-id event)) ERR_NOT_FOUND))
    )
    (asserts! (not (is-eq tx-sender (get submitter event))) ERR_UNAUTHORIZED)
    (asserts! (not (get verified event)) ERR_ALREADY_EXISTS)
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get submitter heritage)) (>= (get reputation-score (default-to {submissions: u0, verifications: u0, reputation-score: u0} (map-get? user-reputation tx-sender))) u100)) ERR_UNAUTHORIZED)
    
    (map-set historical-events event-id
      (merge event {
        verified: true,
        verifier: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (vote-on-event (event-id uint) (support bool))
  (let
    (
      (event (unwrap! (map-get? historical-events event-id) ERR_EVENT_NOT_FOUND))
      (current-votes (default-to {support-votes: u0, dispute-votes: u0, voters: (list), consensus-reached: false} (map-get? event-verification-votes event-id)))
      (voters (get voters current-votes))
    )
    (asserts! (is-none (index-of voters tx-sender)) ERR_ALREADY_EXISTS)
    (asserts! (not (get consensus-reached current-votes)) ERR_ALREADY_EXISTS)
    
    (let
      (
        (new-support-votes (if support (+ (get support-votes current-votes) u1) (get support-votes current-votes)))
        (new-dispute-votes (if support (get dispute-votes current-votes) (+ (get dispute-votes current-votes) u1)))
        (total-votes (+ new-support-votes new-dispute-votes))
        (consensus-threshold u10)
      )
      (map-set event-verification-votes event-id
        {
          support-votes: new-support-votes,
          dispute-votes: new-dispute-votes,
          voters: (unwrap-panic (as-max-len? (append voters tx-sender) u50)),
          consensus-reached: (>= total-votes consensus-threshold)
        }
      )
      (ok true)
    )
  )
)

(define-public (link-related-events (primary-event-id uint) (related-event-id uint))
  (let
    (
      (primary-event (unwrap! (map-get? historical-events primary-event-id) ERR_EVENT_NOT_FOUND))
      (related-event (unwrap! (map-get? historical-events related-event-id) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get submitter primary-event)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get heritage-id primary-event) (get heritage-id related-event)) ERR_INVALID_INPUT)
    
    (let
      (
        (current-related (get related-events primary-event))
      )
      (asserts! (is-none (index-of current-related related-event-id)) ERR_ALREADY_EXISTS)
      
      (map-set historical-events primary-event-id
        (merge primary-event {
          related-events: (unwrap-panic (as-max-len? (append current-related related-event-id) u10))
        })
      )
      (ok true)
    )
  )
)

(define-public (update-event-significance (event-id uint) (new-significance uint))
  (let
    (
      (event (unwrap! (map-get? historical-events event-id) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get submitter event)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (<= new-significance u10) ERR_INVALID_INPUT)
    
    (map-set historical-events event-id
      (merge event {significance-level: new-significance})
    )
    (ok true)
  )
)

(define-public (set-event-submission-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set event-submission-fee new-fee)
    (ok true)
  )
)

(define-private (update-chronological-index (heritage-id uint) (event-date uint) (event-id uint))
  (let
    (
      (date-range (get-date-range event-date))
      (index-key {heritage-id: heritage-id, date-range: date-range})
      (current-events (default-to (list) (map-get? chronological-index index-key)))
    )
    (map-set chronological-index index-key
      (unwrap-panic (as-max-len? (append current-events event-id) u50))
    )
  )
)

(define-private (get-date-range (event-date uint))
  (if (<= event-date u100000)
    "ancient"
    (if (<= event-date u500000)
      "classical"
      (if (<= event-date u1000000)
        "medieval"
        (if (<= event-date u1500000)
          "renaissance"
          (if (<= event-date u2000000)
            "modern"
            "contemporary"
          )
        )
      )
    )
  )
)

(define-read-only (get-historical-event (event-id uint))
  (map-get? historical-events event-id)
)

(define-read-only (get-heritage-timeline (heritage-id uint))
  (map-get? heritage-timeline heritage-id)
)

(define-read-only (get-events-by-period (historical-period (string-ascii 50)))
  (map-get? events-by-period historical-period)
)

(define-read-only (get-events-by-type (event-type (string-ascii 30)))
  (map-get? events-by-type event-type)
)

(define-read-only (get-user-event-submissions (user principal))
  (map-get? user-event-submissions user)
)

(define-read-only (get-event-verification-votes (event-id uint))
  (map-get? event-verification-votes event-id)
)

(define-read-only (get-chronological-events (heritage-id uint) (date-range (string-ascii 20)))
  (map-get? chronological-index {heritage-id: heritage-id, date-range: date-range})
)

(define-read-only (get-event-submission-fee)
  (var-get event-submission-fee)
)

(define-read-only (get-next-event-id)
  (var-get next-event-id)
)

(define-read-only (get-timeline-summary (heritage-id uint))
  (let
    (
      (timeline (default-to (list) (map-get? heritage-timeline heritage-id)))
      (total-events (len timeline))
    )
    (ok {
      total-events: total-events,
      timeline-events: timeline
    })
  )
)

;; Heritage Conservation Fund Functions
(define-public (create-conservation-fund (heritage-id uint) (fund-title (string-ascii 100)) (fund-description (string-ascii 300)) (target-amount uint) (deadline uint) (milestones (list 5 {title: (string-ascii 100), amount: uint})))
  (let
    (
      (fund-id (var-get next-fund-id))
      (creation-fee (var-get fund-creation-fee))
      (heritage (unwrap! (map-get? heritage-registry heritage-id) ERR_NOT_FOUND))
    )
    (asserts! (> (len fund-title) u0) ERR_INVALID_INPUT)
    (asserts! (> (len fund-description) u0) ERR_INVALID_INPUT)
    (asserts! (> target-amount u0) ERR_INVALID_INPUT)
    (asserts! (> deadline stacks-block-height) ERR_INVALID_INPUT)
    (asserts! (> (len milestones) u0) ERR_INVALID_INPUT)
    (asserts! (<= (len milestones) u5) ERR_INVALID_INPUT)
    (asserts! (>= (stx-get-balance tx-sender) creation-fee) ERR_INSUFFICIENT_FUNDS)
    (asserts! (get verified heritage) ERR_UNAUTHORIZED)
    
    (try! (stx-transfer? creation-fee tx-sender CONTRACT_OWNER))
    
    (map-set conservation-funds fund-id
      {
        heritage-id: heritage-id,
        fund-title: fund-title,
        fund-description: fund-description,
        target-amount: target-amount,
        current-amount: u0,
        fund-creator: tx-sender,
        created-at: stacks-block-height,
        deadline: deadline,
        status: "active",
        milestones-count: (len milestones),
        current-milestone: u1
      }
    )
    
    (map-set heritage-fund-list heritage-id
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? heritage-fund-list heritage-id)) fund-id) u10))
    )
    
    (try! (setup-fund-milestones fund-id milestones))
    (var-set next-fund-id (+ fund-id u1))
    (ok fund-id)
  )
)

(define-public (contribute-to-fund (fund-id uint) (amount uint))
  (let
    (
      (fund (unwrap! (map-get? conservation-funds fund-id) ERR_FUND_NOT_FOUND))
      (existing-contribution (map-get? fund-contributions {fund-id: fund-id, contributor: tx-sender}))
    )
    (asserts! (> amount u0) ERR_INVALID_INPUT)
    (asserts! (is-eq (get status fund) "active") ERR_FUND_CLOSED)
    (asserts! (< stacks-block-height (get deadline fund)) ERR_FUND_CLOSED)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (match existing-contribution
      existing-contrib
      (map-set fund-contributions {fund-id: fund-id, contributor: tx-sender}
        {
          amount: (+ (get amount existing-contrib) amount),
          contributed-at: (get contributed-at existing-contrib)
        }
      )
      (map-set fund-contributions {fund-id: fund-id, contributor: tx-sender}
        {
          amount: amount,
          contributed-at: stacks-block-height
        }
      )
    )
    
    (map-set conservation-funds fund-id
      (merge fund {
        current-amount: (+ (get current-amount fund) amount)
      })
    )
    
    (unwrap-panic (update-user-contribution-list tx-sender fund-id amount))
    (unwrap-panic (check-milestone-completion fund-id))
    (ok true)
  )
)

(define-public (complete-milestone (fund-id uint) (milestone-id uint) (evidence-hash (string-ascii 64)))
  (let
    (
      (fund (unwrap! (map-get? conservation-funds fund-id) ERR_FUND_NOT_FOUND))
      (milestone (unwrap! (map-get? fund-milestones {fund-id: fund-id, milestone-id: milestone-id}) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get fund-creator fund)) ERR_UNAUTHORIZED)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_COMPLETED)
    (asserts! (is-eq milestone-id (get current-milestone fund)) ERR_INVALID_INPUT)
    (asserts! (> (len evidence-hash) u0) ERR_INVALID_INPUT)
    
    (map-set fund-milestones {fund-id: fund-id, milestone-id: milestone-id}
      (merge milestone {
        completed: true,
        completion-date: (some stacks-block-height),
        evidence-hash: (some evidence-hash)
      })
    )
    
    (map-set conservation-funds fund-id
      (merge fund {
        current-milestone: (+ (get current-milestone fund) u1)
      })
    )
    
    (if (is-eq milestone-id (get milestones-count fund))
      (map-set conservation-funds fund-id
        (merge fund {status: "completed"})
      )
      true
    )
    (ok true)
  )
)

(define-public (withdraw-fund-excess (fund-id uint))
  (let
    (
      (fund (unwrap! (map-get? conservation-funds fund-id) ERR_FUND_NOT_FOUND))
      (excess-amount (if (> (get current-amount fund) (get target-amount fund))
                      (- (get current-amount fund) (get target-amount fund))
                      u0))
    )
    (asserts! (is-eq tx-sender (get fund-creator fund)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq (get status fund) "completed") (> stacks-block-height (get deadline fund))) ERR_FUND_CLOSED)
    (asserts! (> excess-amount u0) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? excess-amount tx-sender (get fund-creator fund))))
    
    (map-set conservation-funds fund-id
      (merge fund {
        current-amount: (get target-amount fund)
      })
    )
    (ok excess-amount)
  )
)

(define-public (refund-expired-fund (fund-id uint))
  (let
    (
      (fund (unwrap! (map-get? conservation-funds fund-id) ERR_FUND_NOT_FOUND))
      (contribution (unwrap! (map-get? fund-contributions {fund-id: fund-id, contributor: tx-sender}) ERR_NOT_FOUND))
      (refund-amount (get amount contribution))
    )
    (asserts! (> stacks-block-height (get deadline fund)) ERR_FUND_CLOSED)
    (asserts! (not (is-eq (get status fund) "completed")) ERR_FUND_CLOSED)
    (asserts! (> refund-amount u0) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    
    (map-delete fund-contributions {fund-id: fund-id, contributor: tx-sender})
    
    (map-set conservation-funds fund-id
      (merge fund {
        current-amount: (- (get current-amount fund) refund-amount),
        status: "expired"
      })
    )
    (ok refund-amount)
  )
)

(define-public (set-fund-creation-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set fund-creation-fee new-fee)
    (ok true)
  )
)

(define-private (setup-fund-milestones (fund-id uint) (milestones (list 5 {title: (string-ascii 100), amount: uint})))
  (let
    (
      (setup-result (fold setup-milestone-helper milestones {fund-id: fund-id, milestone-id: u1, success: true}))
    )
    (if (get success setup-result)
      (ok true)
      ERR_INVALID_INPUT
    )
  )
)

(define-private (setup-milestone-helper (milestone {title: (string-ascii 100), amount: uint}) (acc {fund-id: uint, milestone-id: uint, success: bool}))
  (if (get success acc)
    (begin
      (map-set fund-milestones {fund-id: (get fund-id acc), milestone-id: (get milestone-id acc)}
        {
          milestone-title: (get title milestone),
          required-amount: (get amount milestone),
          completed: false,
          completion-date: none,
          evidence-hash: none
        }
      )
      {fund-id: (get fund-id acc), milestone-id: (+ (get milestone-id acc) u1), success: true}
    )
    acc
  )
)

(define-private (update-user-contribution-list (contributor principal) (fund-id uint) (amount uint))
  (let
    (
      (current-contributions (default-to (list) (map-get? user-contributions contributor)))
      (existing-entry (get found (find-contribution-entry fund-id current-contributions)))
    )
    (match existing-entry
      entry
      (let
        (
          (updated-contributions (map update-contribution-amount current-contributions))
        )
        (map-set user-contributions contributor updated-contributions)
        (ok true)
      )
      (let
        (
          (new-entry {fund-id: fund-id, amount: amount})
          (updated-contributions (unwrap-panic (as-max-len? (append current-contributions new-entry) u50)))
        )
        (map-set user-contributions contributor updated-contributions)
        (ok true)
      )
    )
  )
)

(define-private (find-contribution-entry (target-fund-id uint) (contributions (list 50 {fund-id: uint, amount: uint})))
  (fold find-contribution-helper contributions {target-fund-id: target-fund-id, found: none})
)

(define-private (find-contribution-helper (contribution {fund-id: uint, amount: uint}) (acc {target-fund-id: uint, found: (optional {fund-id: uint, amount: uint})}))
  (if (is-eq (get fund-id contribution) (get target-fund-id acc))
    {target-fund-id: (get target-fund-id acc), found: (some contribution)}
    acc
  )
)

(define-private (update-contribution-amount (contribution {fund-id: uint, amount: uint}))
  contribution
)

(define-private (check-milestone-completion (fund-id uint))
  (let
    (
      (fund (unwrap! (map-get? conservation-funds fund-id) ERR_FUND_NOT_FOUND))
      (current-milestone-id (get current-milestone fund))
      (milestone (map-get? fund-milestones {fund-id: fund-id, milestone-id: current-milestone-id}))
    )
    (match milestone
      milestone-data
      (if (and (not (get completed milestone-data)) (>= (get current-amount fund) (get required-amount milestone-data)))
        (ok true)
        (ok false)
      )
      (ok false)
    )
  )
)

(define-read-only (get-conservation-fund (fund-id uint))
  (map-get? conservation-funds fund-id)
)

(define-read-only (get-fund-contribution (fund-id uint) (contributor principal))
  (map-get? fund-contributions {fund-id: fund-id, contributor: contributor})
)

(define-read-only (get-fund-milestone (fund-id uint) (milestone-id uint))
  (map-get? fund-milestones {fund-id: fund-id, milestone-id: milestone-id})
)

(define-read-only (get-heritage-funds (heritage-id uint))
  (map-get? heritage-fund-list heritage-id)
)

(define-read-only (get-user-contributions (contributor principal))
  (map-get? user-contributions contributor)
)

(define-read-only (get-fund-creation-fee)
  (var-get fund-creation-fee)
)

(define-read-only (get-next-fund-id)
  (var-get next-fund-id)
)

(define-read-only (get-fund-progress (fund-id uint))
  (match (map-get? conservation-funds fund-id)
    fund-data
    (ok {
      progress-percentage: (if (> (get target-amount fund-data) u0)
                            (/ (* (get current-amount fund-data) u100) (get target-amount fund-data))
                            u0),
      current-amount: (get current-amount fund-data),
      target-amount: (get target-amount fund-data),
      status: (get status fund-data),
      current-milestone: (get current-milestone fund-data),
      total-milestones: (get milestones-count fund-data)
    })
    ERR_FUND_NOT_FOUND
  )
)

(define-read-only (get-fund-statistics (fund-id uint))
  (match (map-get? conservation-funds fund-id)
    fund-data
    (let
      (
        (days-remaining (if (> (get deadline fund-data) stacks-block-height)
                         (- (get deadline fund-data) stacks-block-height)
                         u0))
        (is-active (is-eq (get status fund-data) "active"))
      )
      (ok {
        fund-data: fund-data,
        days-remaining: days-remaining,
        is-expired: (and is-active (is-eq days-remaining u0)),
        funding-rate: (if (> (- stacks-block-height (get created-at fund-data)) u0)
                       (/ (get current-amount fund-data) (- stacks-block-height (get created-at fund-data)))
                       u0)
      })
    )
    ERR_FUND_NOT_FOUND
  )
)




