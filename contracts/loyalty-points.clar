(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-registered (err u103))
(define-constant err-insufficient-points (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-merchant-not-found (err u106))
(define-constant err-already-redeemed (err u107))
(define-constant err-expired (err u108))

(define-data-var points-per-stx uint u10)
(define-data-var min-purchase-amount uint u1000)
(define-data-var redemption-rate uint u100)
(define-data-var total-points-issued uint u0)
(define-data-var total-points-redeemed uint u0)

(define-map merchants 
  { merchant-id: principal }
  { 
    name: (string-ascii 50),
    reward-multiplier: uint,
    is-active: bool
  }
)

(define-map user-points
  { user: principal }
  { 
    balance: uint,
    lifetime-points: uint,
    tier: uint
  }
)

(define-map user-merchants
  { user: principal, merchant: principal }
  { 
    visits: uint,
    last-visit: uint
  }
)

(define-map rewards
  { reward-id: uint }
  {
    merchant: principal,
    points-cost: uint,
    name: (string-ascii 50),
    description: (string-ascii 100),
    expiry: uint,
    is-active: bool
  }
)

(define-map user-rewards
  { user: principal, reward-id: uint }
  {
    redeemed: bool,
    redeemed-at: uint
  }
)

(define-data-var reward-nonce uint u0)

(define-read-only (get-user-points (user principal))
  (default-to 
    { balance: u0, lifetime-points: u0, tier: u0 }
    (map-get? user-points { user: user })
  )
)

(define-read-only (get-merchant (merchant-id principal))
  (map-get? merchants { merchant-id: merchant-id })
)

(define-read-only (get-reward (reward-id uint))
  (map-get? rewards { reward-id: reward-id })
)

(define-read-only (get-user-merchant-stats (user principal) (merchant principal))
  (default-to
    { visits: u0, last-visit: u0 }
    (map-get? user-merchants { user: user, merchant: merchant })
  )
)

(define-read-only (get-user-tier (user principal))
  (get tier (get-user-points user))
)

;; (define-read-only (calculate-points-for-purchase (amount uint) (merchant principal))
;;   (match (get-merchant merchant)
;;     (some merchant-data) (ok (let (
;;       (base-points (/ (* amount (var-get points-per-stx)) u1000))
;;       (multiplier (get reward-multiplier merchant-data))
;;     )
;;       (* base-points multiplier)
;;     ))
;;     none (err err-merchant-not-found)
;;   )
;; )

(define-public (register-merchant (name (string-ascii 50)) (reward-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (is-none (get-merchant tx-sender)) (err err-already-registered))
    (map-set merchants
      { merchant-id: tx-sender }
      { 
        name: name,
        reward-multiplier: reward-multiplier,
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (update-merchant (merchant principal) (name (string-ascii 50)) (reward-multiplier uint) (is-active bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (is-some (get-merchant merchant)) (err err-merchant-not-found))
    (map-set merchants
      { merchant-id: merchant }
      { 
        name: name,
        reward-multiplier: reward-multiplier,
        is-active: is-active
      }
    )
    (ok true)
  )
)

(define-public (award-points (user principal) (amount uint) (purchase-value uint))
  (let (
    (merchant-data (unwrap! (get-merchant tx-sender) (err err-merchant-not-found)))
    (user-data (get-user-points user))
    (user-merchant-data (get-user-merchant-stats user tx-sender))
  )
    (asserts! (get is-active merchant-data) (err err-unauthorized))
    (asserts! (>= purchase-value (var-get min-purchase-amount)) (err err-invalid-amount))
    (asserts! (> amount u0) (err err-invalid-amount))
    
    (map-set user-points
      { user: user }
      {
        balance: (+ (get balance user-data) amount),
        lifetime-points: (+ (get lifetime-points user-data) amount),
        tier: (calculate-tier (+ (get lifetime-points user-data) amount))
      }
    )
    
    (map-set user-merchants
      { user: user, merchant: tx-sender }
      {
        visits: (+ (get visits user-merchant-data) u1),
        last-visit: stacks-block-height
      }
    )
    
    (var-set total-points-issued (+ (var-get total-points-issued) amount))
    (ok true)
  )
)

(define-read-only (calculate-tier (lifetime-points uint))
  (if (< lifetime-points u1000)
    u0
    (if (< lifetime-points u5000)
      u1
      (if (< lifetime-points u20000)
        u2
        u3
      )
    )
  )
)

(define-public (create-reward (points-cost uint) (name (string-ascii 50)) (description (string-ascii 100)) (expiry uint))
  (let (
    (merchant-data (unwrap! (get-merchant tx-sender) (err err-merchant-not-found)))
    (reward-id (+ (var-get reward-nonce) u1))
  )
    (asserts! (get is-active merchant-data) (err err-unauthorized))
    (asserts! (> points-cost u0) (err err-invalid-amount))
    (asserts! (> expiry stacks-block-height) (err err-expired))
    
    (map-set rewards
      { reward-id: reward-id }
      {
        merchant: tx-sender,
        points-cost: points-cost,
        name: name,
        description: description,
        expiry: expiry,
        is-active: true
      }
    )
    
    (var-set reward-nonce reward-id)
    (ok reward-id)
  )
)

(define-public (redeem-reward (reward-id uint))
  (let (
    (reward (unwrap! (get-reward reward-id) (err err-not-found)))
    (user-data (get-user-points tx-sender))
    (user-reward-data (default-to { redeemed: false, redeemed-at: u0 } 
                       (map-get? user-rewards { user: tx-sender, reward-id: reward-id })))
  )
    (asserts! (get is-active reward) (err err-unauthorized))
    (asserts! (not (get redeemed user-reward-data)) (err err-already-redeemed))
    (asserts! (<= (get points-cost reward) (get balance user-data)) (err err-insufficient-points))
    (asserts! (< stacks-block-height (get expiry reward)) (err err-expired))
    
    (map-set user-points
      { user: tx-sender }
      {
        balance: (- (get balance user-data) (get points-cost reward)),
        lifetime-points: (get lifetime-points user-data),
        tier: (get tier user-data)
      }
    )
    
    (map-set user-rewards
      { user: tx-sender, reward-id: reward-id }
      {
        redeemed: true,
        redeemed-at: stacks-block-height
      }
    )
    
    (var-set total-points-redeemed (+ (var-get total-points-redeemed) (get points-cost reward)))
    (ok true)
  )
)

(define-public (update-points-parameters (points-per-stx-new uint) (min-purchase-amount-new uint) (redemption-rate-new uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (var-set points-per-stx points-per-stx-new)
    (var-set min-purchase-amount min-purchase-amount-new)
    (var-set redemption-rate redemption-rate-new)
    (ok true)
  )
)

(define-public (deactivate-reward (reward-id uint))
  (let (
    (reward (unwrap! (get-reward reward-id) (err err-not-found)))
  )
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get merchant reward))) (err err-unauthorized))
    
    (map-set rewards
      { reward-id: reward-id }
      (merge reward { is-active: false })
    )
    (ok true)
  )
)


(define-constant err-self-transfer (err u109))
(define-constant err-recipient-not-found (err u110))

(define-public (transfer-points (recipient principal) (amount uint))
  (let (
    (sender-data (get-user-points tx-sender))
    (recipient-data (get-user-points recipient))
  )
    (asserts! (not (is-eq tx-sender recipient)) (err err-self-transfer))
    (asserts! (>= (get balance sender-data) amount) (err err-insufficient-points))
    (asserts! (> amount u0) (err err-invalid-amount))
    
    (map-set user-points
      { user: tx-sender }
      {
        balance: (- (get balance sender-data) amount),
        lifetime-points: (get lifetime-points sender-data),
        tier: (get tier sender-data)
      }
    )
    
    (map-set user-points 
      { user: recipient }
      {
        balance: (+ (get balance recipient-data) amount),
        lifetime-points: (get lifetime-points recipient-data),
        tier: (get tier recipient-data)
      }
    )
    (ok true)
  )
)


(define-map reward-templates
  { template-id: uint }
  {
    merchant: principal,
    points-cost: uint,
    name: (string-ascii 50),
    description: (string-ascii 100),
    duration: uint
  }
)

(define-data-var template-nonce uint u0)

(define-public (create-reward-template (points-cost uint) (name (string-ascii 50)) (description (string-ascii 100)) (duration uint))
  (let (
    (merchant-data (unwrap! (get-merchant tx-sender) (err err-merchant-not-found)))
    (template-id (+ (var-get template-nonce) u1))
  )
    (asserts! (get is-active merchant-data) (err err-unauthorized))
    (asserts! (> points-cost u0) (err err-invalid-amount))
    (asserts! (> duration u0) (err err-invalid-amount))
    
    (map-set reward-templates
      { template-id: template-id }
      {
        merchant: tx-sender,
        points-cost: points-cost,
        name: name,
        description: description,
        duration: duration
      }
    )
    
    (var-set template-nonce template-id)
    (ok template-id)
  )
)

(define-public (create-reward-from-template (template-id uint))
  (let (
    (template (unwrap! (map-get? reward-templates { template-id: template-id }) (err err-not-found)))
  )
    (asserts! (is-eq tx-sender (get merchant template)) (err err-unauthorized))
    
    (create-reward 
      (get points-cost template)
      (get name template)
      (get description template)
      (+ stacks-block-height (get duration template))
    )
  )
)