;; Claims Processor Smart Contract
;; Automated claim verification and payout system

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u800))
(define-constant err-not-found (err u801))
(define-constant err-invalid-parameters (err u802))
(define-constant err-unauthorized (err u803))
(define-constant err-claim-already-processed (err u804))
(define-constant err-insufficient-coverage (err u805))
(define-constant err-policy-not-active (err u806))
(define-constant max-claim-amount u1000000000) ;; Maximum claim in microSTX
(define-constant claim-review-period u1008) ;; ~7 days for claim review

;; data structures
(define-map insurance-claims uint {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    incident-date: uint,
    claim-type: (string-ascii 30), ;; "accident", "theft", "damage", "medical", "death"
    description: (string-ascii 500),
    evidence-hash: (optional (string-ascii 64)),
    status: (string-ascii 20), ;; "submitted", "under-review", "approved", "rejected", "paid"
    submitted-at: uint,
    reviewed-at: (optional uint),
    reviewer: (optional principal),
    payout-amount: uint,
    rejection-reason: (optional (string-ascii 200))
})

(define-map claim-reviews uint {
    claim-id: uint,
    reviewer: principal,
    review-decision: bool, ;; true = approve, false = reject
    review-notes: (string-ascii 300),
    fraud-score: uint, ;; 0-100, higher = more suspicious
    reviewed-at: uint,
    confidence-level: uint ;; 0-100
})

(define-map fraud-indicators principal {
    total-claims: uint,
    suspicious-claims: uint,
    false-claims: uint,
    fraud-score: uint, ;; 0-1000
    last-claim-date: (optional uint),
    reputation: (string-ascii 20) ;; "trustworthy", "suspicious", "fraudulent"
})

(define-map approved-reviewers principal {
    reviewer-name: (string-ascii 100),
    specialization: (string-ascii 50),
    claims-reviewed: uint,
    accuracy-score: uint, ;; 0-1000
    reputation: uint, ;; 0-1000
    is-active: bool,
    added-at: uint
})

;; data vars
(define-data-var next-claim-id uint u1)
(define-data-var next-review-id uint u1)
(define-data-var total-claims uint u0)
(define-data-var total-payouts uint u0)
(define-data-var total-rejected-claims uint u0)
(define-data-var contract-active bool true)

;; private functions
(define-private (calculate-fraud-risk (claimant principal) (claim-amount uint) (policy-id uint))
    (let ((fraud-data (default-to 
                       {total-claims: u0, suspicious-claims: u0, false-claims: u0,
                        fraud-score: u0, last-claim-date: none, reputation: "trustworthy"}
                       (map-get? fraud-indicators claimant)))
          (claim-frequency-risk (if (is-some (get last-claim-date fraud-data))
                                  (if (< (- stacks-block-height (default-to u0 (get last-claim-date fraud-data))) u4380)
                                      u50 u0) u0))
          (amount-risk (if (> claim-amount u10000000) u30 u0))
          (history-risk (* (get false-claims fraud-data) u20)))
        (+ claim-frequency-risk amount-risk history-risk)))

(define-private (is-authorized-reviewer (reviewer principal))
    (match (map-get? approved-reviewers reviewer)
        reviewer-data (get is-active reviewer-data)
        false))

(define-private (update-fraud-indicators (claimant principal) (suspicious bool))
    (let ((current-data (default-to 
                         {total-claims: u0, suspicious-claims: u0, false-claims: u0,
                          fraud-score: u0, last-claim-date: none, reputation: "trustworthy"}
                         (map-get? fraud-indicators claimant))))
        (map-set fraud-indicators claimant (merge current-data {
            total-claims: (+ (get total-claims current-data) u1),
            suspicious-claims: (if suspicious (+ (get suspicious-claims current-data) u1) (get suspicious-claims current-data)),
            last-claim-date: (some stacks-block-height),
            fraud-score: (if suspicious 
                           (if (> (+ (get fraud-score current-data) u100) u1000) u1000 (+ (get fraud-score current-data) u100))
                           (if (< (get fraud-score current-data) u50) u0 (- (get fraud-score current-data) u50)))
        }))
        (ok true)))

;; public functions
(define-public (submit-claim (policy-id uint) (claim-amount uint) (incident-date uint)
                            (claim-type (string-ascii 30)) (description (string-ascii 500))
                            (evidence-hash (optional (string-ascii 64))))
    (let ((claim-id (var-get next-claim-id))
          (fraud-risk (calculate-fraud-risk tx-sender claim-amount policy-id)))
        
        (asserts! (var-get contract-active) err-unauthorized)
        (asserts! (> claim-amount u0) err-invalid-parameters)
        (asserts! (<= claim-amount max-claim-amount) err-invalid-parameters)
        (asserts! (<= incident-date stacks-block-height) err-invalid-parameters)
        
        ;; Create claim record
        (map-set insurance-claims claim-id {
            policy-id: policy-id,
            claimant: tx-sender,
            claim-amount: claim-amount,
            incident-date: incident-date,
            claim-type: claim-type,
            description: description,
            evidence-hash: evidence-hash,
            status: "submitted",
            submitted-at: stacks-block-height,
            reviewed-at: none,
            reviewer: none,
            payout-amount: u0,
            rejection-reason: none
        })
        
        ;; Fraud indicators would be updated here
        
        ;; Update stats
        (var-set next-claim-id (+ claim-id u1))
        (var-set total-claims (+ (var-get total-claims) u1))
        
        (ok claim-id)))

(define-public (review-claim (claim-id uint) (approve bool) (payout-amount uint) 
                           (review-notes (string-ascii 300)) (rejection-reason (optional (string-ascii 200))))
    (let ((claim-data (unwrap! (map-get? insurance-claims claim-id) err-not-found))
          (review-id (var-get next-review-id)))
        
        (asserts! (is-authorized-reviewer tx-sender) err-unauthorized)
        (asserts! (is-eq (get status claim-data) "submitted") err-claim-already-processed)
        (asserts! (or approve (is-some rejection-reason)) err-invalid-parameters)
        
        ;; Update claim status
        (map-set insurance-claims claim-id (merge claim-data {
            status: (if approve "approved" "rejected"),
            reviewed-at: (some stacks-block-height),
            reviewer: (some tx-sender),
            payout-amount: (if approve payout-amount u0),
            rejection-reason: rejection-reason
        }))
        
        ;; Record review
        (map-set claim-reviews review-id {
            claim-id: claim-id,
            reviewer: tx-sender,
            review-decision: approve,
            review-notes: review-notes,
            fraud-score: (if approve u0 u75),
            reviewed-at: stacks-block-height,
            confidence-level: u85
        })
        
        ;; Update stats
        (var-set next-review-id (+ review-id u1))
        (if approve
            (var-set total-payouts (+ (var-get total-payouts) payout-amount))
            (var-set total-rejected-claims (+ (var-get total-rejected-claims) u1)))
        
        (ok approve)))

(define-public (process-payout (claim-id uint))
    (let ((claim-data (unwrap! (map-get? insurance-claims claim-id) err-not-found)))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (get status claim-data) "approved") err-invalid-parameters)
        (asserts! (> (get payout-amount claim-data) u0) err-invalid-parameters)
        
        ;; Transfer payout to claimant
        (try! (as-contract (stx-transfer? (get payout-amount claim-data) tx-sender (get claimant claim-data))))
        
        ;; Mark as paid
        (map-set insurance-claims claim-id (merge claim-data {
            status: "paid"
        }))
        
        (ok (get payout-amount claim-data))))

(define-public (add-reviewer (reviewer principal) (reviewer-name (string-ascii 100)) (specialization (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? approved-reviewers reviewer)) err-invalid-parameters)
        
        (map-set approved-reviewers reviewer {
            reviewer-name: reviewer-name,
            specialization: specialization,
            claims-reviewed: u0,
            accuracy-score: u500,
            reputation: u500,
            is-active: true,
            added-at: stacks-block-height
        })
        
        (ok true)))

(define-public (appeal-claim (claim-id uint) (appeal-reason (string-ascii 300)))
    (let ((claim-data (unwrap! (map-get? insurance-claims claim-id) err-not-found)))
        (asserts! (is-eq tx-sender (get claimant claim-data)) err-unauthorized)
        (asserts! (is-eq (get status claim-data) "rejected") err-invalid-parameters)
        
        ;; Reset claim for re-review
        (map-set insurance-claims claim-id (merge claim-data {
            status: "under-review",
            reviewed-at: none,
            reviewer: none,
            rejection-reason: none
        }))
        
        (ok true)))

;; read-only functions
(define-read-only (get-claim-details (claim-id uint))
    (map-get? insurance-claims claim-id))

(define-read-only (get-review-details (review-id uint))
    (map-get? claim-reviews review-id))

(define-read-only (get-fraud-indicators (claimant principal))
    (map-get? fraud-indicators claimant))

(define-read-only (get-reviewer-info (reviewer principal))
    (map-get? approved-reviewers reviewer))

(define-read-only (calculate-claim-fraud-risk (claimant principal) (claim-amount uint) (policy-id uint))
    (calculate-fraud-risk claimant claim-amount policy-id))

(define-read-only (get-claims-stats)
    {
        total-claims: (var-get total-claims),
        total-payouts: (var-get total-payouts),
        total-rejected-claims: (var-get total-rejected-claims),
        next-claim-id: (var-get next-claim-id),
        contract-active: (var-get contract-active)
    })

