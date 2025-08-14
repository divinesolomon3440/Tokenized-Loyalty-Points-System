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
    (batch-data (get-user-batch-count user))
    (new-batch-id (+ (get batch-count batch-data) u1))
    (expiry-block (+ stacks-block-height (var-get global-expiry-duration)))
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
    
    (map-set point-expirations
      { user: user, batch-id: new-batch-id }
      {
        points: amount,
        expiry-block: expiry-block,
        is-expired: false
      }
    )
    
    (map-set user-point-batches
      { user: user }
      { batch-count: new-batch-id }
    )
    
    (var-set total-points-issued (+ (var-get total-points-issued) amount))
    (ok true)
  )
)

(define-public (expire-point-batch (user principal) (batch-id uint))
  (let (
    (batch (unwrap! (get-point-expiration user batch-id) (err err-not-found)))
    (user-data (get-user-points user))
  )
    (asserts! (< (get expiry-block batch) stacks-block-height) (err err-invalid-expiry))
    (asserts! (not (get is-expired batch)) (err err-already-redeemed))
    
    (map-set point-expirations
      { user: user, batch-id: batch-id }
      (merge batch { is-expired: true })
    )
    
    (map-set user-points
      { user: user }
      {
        balance: (- (get balance user-data) (get points batch)),
        lifetime-points: (get lifetime-points user-data),
        tier: (get tier user-data)
      }
    )
    
    (ok (get points batch))
  )
)

(define-public (set-expiry-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (> new-duration u0) (err err-invalid-amount))
    (var-set global-expiry-duration new-duration)
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
    (merchant-perf (get-merchant-performance tx-sender))
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
    
    ;; Initialize pricing data for new reward
    (map-set reward-pricing
      { reward-id: reward-id }
      {
        demand-score: u0,
        redemption-count: u0,
        last-updated: stacks-block-height,
        current-multiplier: u100
      }
    )
    
    ;; Update merchant performance metrics
    (map-set merchant-performance
      { merchant: tx-sender }
      {
        total-rewards-created: (+ (get total-rewards-created merchant-perf) u1),
        total-redemptions: (get total-redemptions merchant-perf),
        performance-score: (get performance-score merchant-perf),
        last-updated: stacks-block-height
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
    (dynamic-price (unwrap! (calculate-dynamic-price reward-id tx-sender) (err err-invalid-amount)))
    (pricing-data (get-reward-pricing reward-id))
    (merchant-perf (get-merchant-performance (get merchant reward)))
  )
    (asserts! (get is-active reward) (err err-unauthorized))
    (asserts! (not (get redeemed user-reward-data)) (err err-already-redeemed))
    (asserts! (<= dynamic-price (get balance user-data)) (err err-insufficient-points))
    (asserts! (< stacks-block-height (get expiry reward)) (err err-expired))
    
    (map-set user-points
      { user: tx-sender }
      {
        balance: (- (get balance user-data) dynamic-price),
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
    
    ;; Update reward pricing based on redemption
    (map-set reward-pricing
      { reward-id: reward-id }
      {
        demand-score: (+ (get demand-score pricing-data) u10),
        redemption-count: (+ (get redemption-count pricing-data) u1),
        last-updated: stacks-block-height,
        current-multiplier: (if (> (+ (get current-multiplier pricing-data) u5) (var-get max-price-multiplier))
                              (var-get max-price-multiplier)
                              (+ (get current-multiplier pricing-data) u5))
      }
    )
    
    ;; Update merchant performance
    (map-set merchant-performance
      { merchant: (get merchant reward) }
      {
        total-rewards-created: (get total-rewards-created merchant-perf),
        total-redemptions: (+ (get total-redemptions merchant-perf) u1),
        performance-score: (if (> (+ (get performance-score merchant-perf) u2) u150)
                            u150
                            (+ (get performance-score merchant-perf) u2)),
        last-updated: stacks-block-height
      }
    )
    
    (var-set total-points-redeemed (+ (var-get total-points-redeemed) dynamic-price))
    (ok true)
  )
)

;; Dynamic Pricing Engine Management Functions
(define-public (configure-pricing-engine (enabled bool) (base-multiplier uint) (max-multiplier uint) (min-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (and (> max-multiplier min-multiplier) (>= min-multiplier u25) (<= max-multiplier u500)) (err err-invalid-multiplier))
    (var-set pricing-enabled enabled)
    (var-set base-demand-multiplier base-multiplier)
    (var-set max-price-multiplier max-multiplier)
    (var-set min-price-multiplier min-multiplier)
    (ok true)
  )
)

(define-public (set-tier-discount (tier uint) (discount-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (and (<= tier u3) (>= discount-multiplier u50) (<= discount-multiplier u100)) (err err-invalid-multiplier))
    (map-set tier-discounts { tier: tier } { discount-multiplier: discount-multiplier })
    (ok true)
  )
)

(define-public (adjust-reward-pricing (reward-id uint) (new-multiplier uint))
  (let (
    (reward (unwrap! (get-reward reward-id) (err err-not-found)))
    (pricing-data (get-reward-pricing reward-id))
  )
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get merchant reward))) (err err-unauthorized))
    (asserts! (and (>= new-multiplier (var-get min-price-multiplier)) (<= new-multiplier (var-get max-price-multiplier))) (err err-invalid-multiplier))
    
    (map-set reward-pricing
      { reward-id: reward-id }
      {
        demand-score: (get demand-score pricing-data),
        redemption-count: (get redemption-count pricing-data),
        last-updated: stacks-block-height,
        current-multiplier: new-multiplier
      }
    )
    (ok true)
  )
)

(define-public (decay-reward-demand (reward-id uint))
  (let (
    (pricing-data (get-reward-pricing reward-id))
    (blocks-since-update (- stacks-block-height (get last-updated pricing-data)))
    (decay-amount (/ blocks-since-update u1000))
    (new-multiplier (if (< (- (get current-multiplier pricing-data) decay-amount) (var-get min-price-multiplier))
                     (var-get min-price-multiplier)
                     (- (get current-multiplier pricing-data) decay-amount)))
  )
    (map-set reward-pricing
      { reward-id: reward-id }
      {
        demand-score: (if (< (- (get demand-score pricing-data) decay-amount) u0)
                       u0
                       (- (get demand-score pricing-data) decay-amount)),
        redemption-count: (get redemption-count pricing-data),
        last-updated: stacks-block-height,
        current-multiplier: new-multiplier
      }
    )
    (ok new-multiplier)
  )
)

(define-public (bulk-decay-rewards (reward-ids (list 10 uint)))
  (begin
    (asserts! (var-get pricing-enabled) (err err-pricing-disabled))
    (ok (map decay-reward-demand reward-ids))
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

(define-constant err-partnership-exists (err u204))
(define-constant err-self-partnership (err u205))
(define-constant err-inactive-partnership (err u206))
(define-constant err-window-expired (err u207))
(define-constant err-invalid-expiry (err u208))
(define-constant err-invalid-multiplier (err u209))
(define-constant err-pricing-disabled (err u210))

(define-data-var partnership-nonce uint u0)
(define-data-var global-expiry-duration uint u52560)

(define-map point-expirations
  { user: principal, batch-id: uint }
  {
    points: uint,
    expiry-block: uint,
    is-expired: bool
  }
)

(define-map user-point-batches
  { user: principal }
  { batch-count: uint }
)

;; Dynamic Pricing Engine Data Structures
(define-data-var pricing-enabled bool true)
(define-data-var base-demand-multiplier uint u100)
(define-data-var max-price-multiplier uint u200)
(define-data-var min-price-multiplier uint u50)

(define-map reward-pricing
  { reward-id: uint }
  {
    demand-score: uint,
    redemption-count: uint,
    last-updated: uint,
    current-multiplier: uint
  }
)

(define-map tier-discounts
  { tier: uint }
  { discount-multiplier: uint }
)

(define-map merchant-performance
  { merchant: principal }
  {
    total-rewards-created: uint,
    total-redemptions: uint,
    performance-score: uint,
    last-updated: uint
  }
)

(define-map partnerships
  { partnership-id: uint }
  {
    merchant-a: principal,
    merchant-b: principal,
    bonus-multiplier: uint,
    time-window: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map merchant-partnerships
  { merchant: principal, partner: principal }
  { partnership-id: uint }
)

(define-map user-partnership-progress
  { user: principal, partnership-id: uint }
  {
    merchant-a-last-purchase: uint,
    merchant-b-last-purchase: uint,
    total-bonus-earned: uint
  }
)

(define-read-only (get-partnership (partnership-id uint))
  (map-get? partnerships { partnership-id: partnership-id })
)

(define-read-only (get-merchant-partnership (merchant principal) (partner principal))
  (map-get? merchant-partnerships { merchant: merchant, partner: partner })
)

(define-read-only (get-user-partnership-progress (user principal) (partnership-id uint))
  (default-to
    { merchant-a-last-purchase: u0, merchant-b-last-purchase: u0, total-bonus-earned: u0 }
    (map-get? user-partnership-progress { user: user, partnership-id: partnership-id })
  )
)

;; Dynamic Pricing Engine Read-Only Functions
(define-read-only (get-reward-pricing (reward-id uint))
  (default-to
    { demand-score: u0, redemption-count: u0, last-updated: u0, current-multiplier: u100 }
    (map-get? reward-pricing { reward-id: reward-id })
  )
)

(define-read-only (get-tier-discount (tier uint))
  (default-to { discount-multiplier: u100 } (map-get? tier-discounts { tier: tier }))
)

(define-read-only (get-merchant-performance (merchant principal))
  (default-to
    { total-rewards-created: u0, total-redemptions: u0, performance-score: u100, last-updated: u0 }
    (map-get? merchant-performance { merchant: merchant })
  )
)

(define-read-only (calculate-dynamic-price (reward-id uint) (user principal))
  (let (
    (reward (unwrap! (get-reward reward-id) (err err-not-found)))
    (pricing-data (get-reward-pricing reward-id))
    (user-tier (get-user-tier user))
    (tier-discount (get-tier-discount user-tier))
    (base-cost (get points-cost reward))
    (demand-multiplier (get current-multiplier pricing-data))
    (discount-multiplier (get discount-multiplier tier-discount))
  )
    (if (var-get pricing-enabled)
      (ok (/ (* (* base-cost demand-multiplier) discount-multiplier) (* u100 u100)))
      (ok base-cost)
    )
  )
)

(define-read-only (get-pricing-status)
  {
    enabled: (var-get pricing-enabled),
    base-multiplier: (var-get base-demand-multiplier),
    max-multiplier: (var-get max-price-multiplier),
    min-multiplier: (var-get min-price-multiplier)
  }
)

(define-read-only (get-point-expiration (user principal) (batch-id uint))
  (map-get? point-expirations { user: user, batch-id: batch-id })
)

(define-read-only (get-user-batch-count (user principal))
  (default-to { batch-count: u0 } (map-get? user-point-batches { user: user }))
)

(define-read-only (calculate-active-points (user principal))
  (let (
    (batch-data (get-user-batch-count user))
    (total-batches (get batch-count batch-data))
  )
    (fold calculate-active-batch-points 
      (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
      { user: user, active-points: u0, current-batch: u1, total-batches: total-batches }
    )
  )
)

(define-private (calculate-active-batch-points (batch-num uint) (acc { user: principal, active-points: uint, current-batch: uint, total-batches: uint }))
  (if (<= (get current-batch acc) (get total-batches acc))
    (match (get-point-expiration (get user acc) (get current-batch acc))
      some-batch (if (and 
                      (not (get is-expired some-batch))
                      (< stacks-block-height (get expiry-block some-batch)))
                   {
                     user: (get user acc),
                     active-points: (+ (get active-points acc) (get points some-batch)),
                     current-batch: (+ (get current-batch acc) u1),
                     total-batches: (get total-batches acc)
                   }
                   {
                     user: (get user acc),
                     active-points: (get active-points acc),
                     current-batch: (+ (get current-batch acc) u1),
                     total-batches: (get total-batches acc)
                   })
      {
        user: (get user acc),
        active-points: (get active-points acc),
        current-batch: (+ (get current-batch acc) u1),
        total-batches: (get total-batches acc)
      }
    )
    acc
  )
)

(define-read-only (check-partnership-eligibility (user principal) (partnership-id uint))
  (let (
    (partnership (unwrap! (get-partnership partnership-id) (err err-not-found)))
    (progress (get-user-partnership-progress user partnership-id))
    (current-block stacks-block-height)
    (time-window (get time-window partnership))
    (merchant-a-purchase (get merchant-a-last-purchase progress))
    (merchant-b-purchase (get merchant-b-last-purchase progress))
  )
    (ok (and
      (get is-active partnership)
      (> merchant-a-purchase u0)
      (> merchant-b-purchase u0)
      (<= (- current-block merchant-a-purchase) time-window)
      (<= (- current-block merchant-b-purchase) time-window)
    ))
  )
)

(define-public (create-partnership (partner principal) (bonus-multiplier uint) (time-window uint))
  (let (
    (partnership-id (+ (var-get partnership-nonce) u1))
  )
    (asserts! (not (is-eq tx-sender partner)) (err err-self-partnership))
    (asserts! (> bonus-multiplier u0) (err err-invalid-amount))
    (asserts! (> time-window u0) (err err-invalid-amount))
    (asserts! (is-none (get-merchant-partnership tx-sender partner)) (err err-partnership-exists))
    (asserts! (is-none (get-merchant-partnership partner tx-sender)) (err err-partnership-exists))
    
    (map-set partnerships
      { partnership-id: partnership-id }
      {
        merchant-a: tx-sender,
        merchant-b: partner,
        bonus-multiplier: bonus-multiplier,
        time-window: time-window,
        is-active: false,
        created-at: stacks-block-height
      }
    )
    
    (map-set merchant-partnerships
      { merchant: tx-sender, partner: partner }
      { partnership-id: partnership-id }
    )
    
    (map-set merchant-partnerships
      { merchant: partner, partner: tx-sender }
      { partnership-id: partnership-id }
    )
    
    (var-set partnership-nonce partnership-id)
    (ok partnership-id)
  )
)

(define-public (accept-partnership (partnership-id uint))
  (let (
    (partnership (unwrap! (get-partnership partnership-id) (err err-not-found)))
  )
    (asserts! (is-eq tx-sender (get merchant-b partnership)) (err err-unauthorized))
    (asserts! (not (get is-active partnership)) (err err-partnership-exists))
    
    (map-set partnerships
      { partnership-id: partnership-id }
      (merge partnership { is-active: true })
    )
    (ok true)
  )
)


(define-private (get-merchant-partnerships-list (merchant principal))
  (list)
)

(define-private (record-single-partnership-purchase (partnership-data { partner: principal, partnership-id: uint }) (prev-result (response principal uint)))
  (match prev-result
    ok-user (let (
      (user ok-user)
      (partnership-id (get partnership-id partnership-data))
      (partnership (unwrap! (get-partnership partnership-id) prev-result))
      (progress (get-user-partnership-progress user partnership-id))
      (is-merchant-a (is-eq tx-sender (get merchant-a partnership)))
      (is-merchant-b (is-eq tx-sender (get merchant-b partnership)))
    )
      (if (get is-active partnership)
        (begin
          (map-set user-partnership-progress
            { user: user, partnership-id: partnership-id }
            {
              merchant-a-last-purchase: (if is-merchant-a stacks-block-height (get merchant-a-last-purchase progress)),
              merchant-b-last-purchase: (if is-merchant-b stacks-block-height (get merchant-b-last-purchase progress)),
              total-bonus-earned: (get total-bonus-earned progress)
            }
          )
          (ok user)
        )
        (ok user)
      )
    )
    err-val prev-result
  )
)

(define-public (claim-partnership-bonus (partnership-id uint) (base-points uint))
  (let (
    (partnership (unwrap! (get-partnership partnership-id) (err err-not-found)))
    (progress (get-user-partnership-progress tx-sender partnership-id))
    (eligible (unwrap! (check-partnership-eligibility tx-sender partnership-id) (err err-window-expired)))
    (bonus-points (* base-points (get bonus-multiplier partnership)))
  )
    (asserts! eligible (err err-window-expired))
    (asserts! (> base-points u0) (err err-invalid-amount))
    
    (map-set user-partnership-progress
      { user: tx-sender, partnership-id: partnership-id }
      {
        merchant-a-last-purchase: u0,
        merchant-b-last-purchase: u0,
        total-bonus-earned: (+ (get total-bonus-earned progress) bonus-points)
      }
    )
    
    (ok bonus-points)
  )
)

(define-public (deactivate-partnership (partnership-id uint))
  (let (
    (partnership (unwrap! (get-partnership partnership-id) (err err-not-found)))
  )
    (asserts! (or 
      (is-eq tx-sender (get merchant-a partnership))
      (is-eq tx-sender (get merchant-b partnership))
      (is-eq tx-sender contract-owner)
    ) (err err-unauthorized))
    
    (map-set partnerships
      { partnership-id: partnership-id }
      (merge partnership { is-active: false })
    )
    (ok true)
  )
)

(define-public (update-partnership (partnership-id uint) (bonus-multiplier uint) (time-window uint))
  (let (
    (partnership (unwrap! (get-partnership partnership-id) (err err-not-found)))
  )
    (asserts! (or 
      (is-eq tx-sender (get merchant-a partnership))
      (is-eq tx-sender (get merchant-b partnership))
    ) (err err-unauthorized))
    (asserts! (> bonus-multiplier u0) (err err-invalid-amount))
    (asserts! (> time-window u0) (err err-invalid-amount))
    
    (map-set partnerships
      { partnership-id: partnership-id }
      (merge partnership { 
        bonus-multiplier: bonus-multiplier,
        time-window: time-window
      })
    )
    (ok true)
  )
)


(define-public (award-points-with-partnerships (user principal) (amount uint) (purchase-value uint))
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
    
    ;; (try! (contract-call? .merchant-partnerships record-partnership-purchase user tx-sender))
    
    (ok true)
  )
)



