;; Analytics Dashboard for Loyalty Points System
;; Provides comprehensive reporting and analytics capabilities
;; All functions are public to enable contract calls from the main contract

(define-constant CONTRACT-LOYALTY .loyalty-points)

;; Error constants
(define-constant ERR-CONTRACT-CALL-FAILED (err u600))
(define-constant ERR-MERCHANT-NOT-FOUND (err u601))
(define-constant ERR-USER-NOT-FOUND (err u602))
(define-constant ERR-INVALID-PARAMETERS (err u603))

;; Get merchant analytics data
(define-public (get-merchant-stats (merchant principal))
  (let (
    (merchant-data (unwrap! (contract-call? CONTRACT-LOYALTY get-merchant merchant) ERR-MERCHANT-NOT-FOUND))
    (performance-data (contract-call? CONTRACT-LOYALTY get-merchant-performance merchant))
  )
    (ok {
      merchant: merchant,
      name: (get name merchant-data),
      is-active: (get is-active merchant-data),
      reward-multiplier: (get reward-multiplier merchant-data),
      total-rewards-created: (get total-rewards-created performance-data),
      total-redemptions: (get total-redemptions performance-data),
      performance-score: (get performance-score performance-data),
      last-updated: (get last-updated performance-data)
    })
  )
)

;; Get comprehensive user analytics
(define-public (get-user-stats (user principal))
  (let (
    (user-points (contract-call? CONTRACT-LOYALTY get-user-points user))
    (user-tier (contract-call? CONTRACT-LOYALTY get-user-tier user))
    (batch-data (contract-call? CONTRACT-LOYALTY get-user-batch-count user))
    (active-points-data (contract-call? CONTRACT-LOYALTY calculate-active-points user))
  )
    (ok {
      user: user,
      current-balance: (get balance user-points),
      lifetime-points: (get lifetime-points user-points),
      tier: user-tier,
      total-point-batches: (get batch-count batch-data),
      active-points: (get active-points active-points-data),
      expired-points: (- (get lifetime-points user-points) (get active-points active-points-data))
    })
  )
)

;; Get system-wide analytics
(define-public (get-system-stats)
  (let (
    (pricing-status (contract-call? CONTRACT-LOYALTY get-pricing-status))
  )
    (ok {
      pricing-enabled: (get enabled pricing-status),
      base-multiplier: (get base-multiplier pricing-status),
      max-multiplier: (get max-multiplier pricing-status),
      min-multiplier: (get min-multiplier pricing-status),
      current-block: stacks-block-height
    })
  )
)

;; Get user engagement metrics with a merchant
(define-public (get-user-merchant-engagement (user principal) (merchant principal))
  (let (
    (engagement-data (contract-call? CONTRACT-LOYALTY get-user-merchant-stats user merchant))
  )
    (ok {
      user: user,
      merchant: merchant,
      total-visits: (get visits engagement-data),
      last-visit-block: (get last-visit engagement-data),
      blocks-since-visit: (if (> (get last-visit engagement-data) u0)
                           (- stacks-block-height (get last-visit engagement-data))
                           u0)
    })
  )
)

;; Get tier information and discount details
(define-public (get-tier-analytics (tier uint))
  (let (
    (tier-discount (contract-call? CONTRACT-LOYALTY get-tier-discount tier))
  )
    (ok {
      tier: tier,
      discount-multiplier: (get discount-multiplier tier-discount),
      tier-name: (if (is-eq tier u0) 
                   "Bronze"
                   (if (is-eq tier u1)
                     "Silver"
                     (if (is-eq tier u2)
                       "Gold"
                       "Platinum")))
    })
  )
)

;; Get reward analytics with pricing data
(define-public (get-reward-analytics (reward-id uint))
  (let (
    (reward-data (unwrap! (contract-call? CONTRACT-LOYALTY get-reward reward-id) ERR-MERCHANT-NOT-FOUND))
    (pricing-data (contract-call? CONTRACT-LOYALTY get-reward-pricing reward-id))
    (dynamic-price (unwrap! (contract-call? CONTRACT-LOYALTY calculate-dynamic-price reward-id tx-sender) ERR-CONTRACT-CALL-FAILED))
  )
    (ok {
      reward-id: reward-id,
      merchant: (get merchant reward-data),
      name: (get name reward-data),
      description: (get description reward-data),
      base-cost: (get points-cost reward-data),
      current-price: dynamic-price,
      is-active: (get is-active reward-data),
      expiry: (get expiry reward-data),
      demand-score: (get demand-score pricing-data),
      redemption-count: (get redemption-count pricing-data),
      current-multiplier: (get current-multiplier pricing-data)
    })
  )
)

;; Get partnership analytics
(define-public (get-partnership-analytics (partnership-id uint))
  (let (
    (partnership-data (unwrap! (contract-call? CONTRACT-LOYALTY get-partnership partnership-id) ERR-MERCHANT-NOT-FOUND))
  )
    (ok {
      partnership-id: partnership-id,
      merchant-a: (get merchant-a partnership-data),
      merchant-b: (get merchant-b partnership-data),
      bonus-multiplier: (get bonus-multiplier partnership-data),
      time-window: (get time-window partnership-data),
      is-active: (get is-active partnership-data),
      created-at: (get created-at partnership-data)
    })
  )
)

;; Get user point expiration schedule
(define-public (get-user-expiration-schedule (user principal) (batch-id uint))
  (let (
    (expiration-data (unwrap! (contract-call? CONTRACT-LOYALTY get-point-expiration user batch-id) ERR-USER-NOT-FOUND))
  )
    (ok {
      user: user,
      batch-id: batch-id,
      points: (get points expiration-data),
      expiry-block: (get expiry-block expiration-data),
      is-expired: (get is-expired expiration-data),
      blocks-until-expiry: (if (> (get expiry-block expiration-data) stacks-block-height)
                            (- (get expiry-block expiration-data) stacks-block-height)
                            u0)
    })
  )
)

;; Get comprehensive dashboard summary
(define-public (get-dashboard-summary)
  (let (
    (pricing-status (contract-call? CONTRACT-LOYALTY get-pricing-status))
  )
    (ok {
      system: {
        pricing-enabled: (get enabled pricing-status),
        base-multiplier: (get base-multiplier pricing-status),
        max-multiplier: (get max-multiplier pricing-status),
        min-multiplier: (get min-multiplier pricing-status),
        current-block: stacks-block-height
      },
      dashboard-version: "v1.0",
      last-updated: stacks-block-height,
      total-functions: u9
    })
  )
)
