;; Policy Management Smart Contract
;; Create and manage insurance policies and premiums

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u700))
(define-constant err-not-found (err u701))
(define-constant err-invalid-parameters (err u702))
(define-constant err-unauthorized (err u703))
(define-constant err-policy-expired (err u704))
(define-constant err-insufficient-premium (err u705))
(define-constant max-coverage-amount u1000000000) ;; Maximum coverage in microSTX
(define-constant min-premium-rate u1) ;; Minimum premium rate (basis points)

;; data structures
(define-map insurance-policies uint {
    policy-holder: principal,
    policy-type: (string-ascii 30), ;; "health", "auto", "property", "life"
    coverage-amount: uint,
    premium-amount: uint,
    premium-frequency: (string-ascii 20), ;; "monthly", "quarterly", "annual"
    start-date: uint,
    end-date: uint,
    status: (string-ascii 15), ;; "active", "expired", "cancelled", "claimed"
    risk-score: uint, ;; 0-1000 risk assessment
    total-paid-premiums: uint,
    last-premium-payment: (optional uint),
    next-premium-due: uint,
    created-at: uint
})

(define-map policy-holders principal {
    total-policies: uint,
    active-policies: uint,
    total-claims: uint,
    total-premiums-paid: uint,
    reputation-score: uint, ;; 0-1000
    risk-profile: (string-ascii 20), ;; "low", "medium", "high"
    joined-at: uint,
    last-activity: uint
})

(define-map premium-payments uint {
    policy-id: uint,
    payment-amount: uint,
    payment-date: uint,
    payment-type: (string-ascii 20), ;; "initial", "monthly", "quarterly", "annual"
    late-payment: bool,
    penalty-amount: uint
})

(define-map risk-factors (string-ascii 30) {
    base-rate: uint, ;; basis points
    max-coverage: uint,
    min-coverage: uint,
    assessment-criteria: (string-ascii 200),
    active: bool
})

;; data vars
(define-data-var next-policy-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var total-policies uint u0)
(define-data-var total-active-policies uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var contract-active bool true)

;; private functions
(define-private (calculate-premium (coverage-amount uint) (risk-score uint) (policy-type (string-ascii 30)))
    (let ((base-rate (default-to u100 (get base-rate (map-get? risk-factors policy-type))))
          (risk-multiplier (+ u100 (/ (* risk-score u50) u1000))))
        (/ (* coverage-amount base-rate risk-multiplier) u1000000)))

(define-private (is-premium-due (policy-id uint))
    (match (map-get? insurance-policies policy-id)
        policy-data (<= (get next-premium-due policy-data) stacks-block-height)
        false))

(define-private (update-policy-holder-stats (holder principal) (premium-amount uint))
    (let ((holder-data (default-to 
                        {total-policies: u0, active-policies: u0, total-claims: u0,
                         total-premiums-paid: u0, reputation-score: u500, risk-profile: "medium",
                         joined-at: stacks-block-height, last-activity: stacks-block-height}
                        (map-get? policy-holders holder))))
        (map-set policy-holders holder (merge holder-data {
            total-premiums-paid: (+ (get total-premiums-paid holder-data) premium-amount),
            reputation-score: (if (> (+ (get reputation-score holder-data) u5) u1000) u1000 (+ (get reputation-score holder-data) u5)),
            last-activity: stacks-block-height
        }))))

;; public functions
(define-public (create-policy (policy-type (string-ascii 30)) (coverage-amount uint) 
                             (premium-frequency (string-ascii 20)) (risk-score uint))
    (let ((policy-id (var-get next-policy-id))
          (premium-amount (calculate-premium coverage-amount risk-score policy-type))
          (duration-blocks (if (is-eq premium-frequency "annual") u52560 
                             (if (is-eq premium-frequency "monthly") u4380 u13140))))
        
        (asserts! (var-get contract-active) err-unauthorized)
        (asserts! (> coverage-amount u0) err-invalid-parameters)
        (asserts! (<= coverage-amount max-coverage-amount) err-invalid-parameters)
        (asserts! (<= risk-score u1000) err-invalid-parameters)
        (asserts! (or (is-eq premium-frequency "monthly")
                     (is-eq premium-frequency "quarterly")
                     (is-eq premium-frequency "annual")) err-invalid-parameters)
        
        ;; Transfer initial premium
        (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
        
        ;; Create policy
        (map-set insurance-policies policy-id {
            policy-holder: tx-sender,
            policy-type: policy-type,
            coverage-amount: coverage-amount,
            premium-amount: premium-amount,
            premium-frequency: premium-frequency,
            start-date: stacks-block-height,
            end-date: (+ stacks-block-height duration-blocks),
            status: "active",
            risk-score: risk-score,
            total-paid-premiums: premium-amount,
            last-premium-payment: (some stacks-block-height),
            next-premium-due: (+ stacks-block-height (if (is-eq premium-frequency "monthly") u4380
                                                       (if (is-eq premium-frequency "quarterly") u13140 u52560))),
            created-at: stacks-block-height
        })
        
        ;; Record premium payment
        (map-set premium-payments (var-get next-payment-id) {
            policy-id: policy-id,
            payment-amount: premium-amount,
            payment-date: stacks-block-height,
            payment-type: "initial",
            late-payment: false,
            penalty-amount: u0
        })
        
        ;; Update stats
        (var-set next-policy-id (+ policy-id u1))
        (var-set next-payment-id (+ (var-get next-payment-id) u1))
        (var-set total-policies (+ (var-get total-policies) u1))
        (var-set total-active-policies (+ (var-get total-active-policies) u1))
        (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-amount))
        
        (update-policy-holder-stats tx-sender premium-amount)
        
        (ok policy-id)))

(define-public (pay-premium (policy-id uint))
    (let ((policy-data (unwrap! (map-get? insurance-policies policy-id) err-not-found))
          (premium-amount (get premium-amount policy-data))
          (is-late (> stacks-block-height (get next-premium-due policy-data)))
          (penalty (if is-late (/ premium-amount u10) u0))
          (total-payment (+ premium-amount penalty)))
        
        (asserts! (is-eq tx-sender (get policy-holder policy-data)) err-unauthorized)
        (asserts! (is-eq (get status policy-data) "active") err-policy-expired)
        
        ;; Transfer premium payment
        (try! (stx-transfer? total-payment tx-sender (as-contract tx-sender)))
        
        ;; Update policy
        (map-set insurance-policies policy-id (merge policy-data {
            total-paid-premiums: (+ (get total-paid-premiums policy-data) total-payment),
            last-premium-payment: (some stacks-block-height),
            next-premium-due: (+ stacks-block-height 
                                (if (is-eq (get premium-frequency policy-data) "monthly") u4380
                                  (if (is-eq (get premium-frequency policy-data) "quarterly") u13140 u52560)))
        }))
        
        ;; Record payment
        (map-set premium-payments (var-get next-payment-id) {
            policy-id: policy-id,
            payment-amount: total-payment,
            payment-date: stacks-block-height,
            payment-type: (get premium-frequency policy-data),
            late-payment: is-late,
            penalty-amount: penalty
        })
        
        (var-set next-payment-id (+ (var-get next-payment-id) u1))
        (var-set total-premiums-collected (+ (var-get total-premiums-collected) total-payment))
        
        (update-policy-holder-stats tx-sender total-payment)
        
        (ok true)))

(define-public (cancel-policy (policy-id uint))
    (let ((policy-data (unwrap! (map-get? insurance-policies policy-id) err-not-found)))
        (asserts! (is-eq tx-sender (get policy-holder policy-data)) err-unauthorized)
        (asserts! (is-eq (get status policy-data) "active") err-policy-expired)
        
        (map-set insurance-policies policy-id (merge policy-data {
            status: "cancelled"
        }))
        
        (var-set total-active-policies (- (var-get total-active-policies) u1))
        
        (ok true)))

(define-public (renew-policy (policy-id uint))
    (let ((policy-data (unwrap! (map-get? insurance-policies policy-id) err-not-found))
          (new-end-date (+ (get end-date policy-data) u52560)))
        
        (asserts! (is-eq tx-sender (get policy-holder policy-data)) err-unauthorized)
        (asserts! (or (is-eq (get status policy-data) "active")
                     (is-eq (get status policy-data) "expired")) err-invalid-parameters)
        
        (map-set insurance-policies policy-id (merge policy-data {
            end-date: new-end-date,
            status: "active"
        }))
        
        (ok true)))

;; read-only functions
(define-read-only (get-policy-details (policy-id uint))
    (map-get? insurance-policies policy-id))

(define-read-only (get-policy-holder-info (holder principal))
    (map-get? policy-holders holder))

(define-read-only (get-payment-details (payment-id uint))
    (map-get? premium-payments payment-id))

(define-read-only (calculate-policy-premium (coverage-amount uint) (risk-score uint) (policy-type (string-ascii 30)))
    (calculate-premium coverage-amount risk-score policy-type))

(define-read-only (is-policy-active (policy-id uint))
    (match (map-get? insurance-policies policy-id)
        policy-data (and (is-eq (get status policy-data) "active")
                        (< stacks-block-height (get end-date policy-data)))
        false))

(define-read-only (get-contract-stats)
    {
        total-policies: (var-get total-policies),
        total-active-policies: (var-get total-active-policies),
        total-premiums-collected: (var-get total-premiums-collected),
        next-policy-id: (var-get next-policy-id),
        contract-active: (var-get contract-active)
    })

