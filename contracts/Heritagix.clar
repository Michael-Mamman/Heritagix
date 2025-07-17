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

(define-data-var next-heritage-id uint u1)
(define-data-var registration-fee uint u1000000)
(define-data-var verification-reward uint u500000)
(define-data-var next-media-id uint u1)
(define-data-var media-upload-fee uint u100000)
(define-data-var max-media-size uint u10485760)

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