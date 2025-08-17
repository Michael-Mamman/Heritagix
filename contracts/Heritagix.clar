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

(define-data-var next-heritage-id uint u1)
(define-data-var registration-fee uint u1000000)
(define-data-var verification-reward uint u500000)
(define-data-var next-media-id uint u1)
(define-data-var media-upload-fee uint u100000)
(define-data-var max-media-size uint u10485760)
(define-data-var next-event-id uint u1)
(define-data-var event-submission-fee uint u50000)

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




