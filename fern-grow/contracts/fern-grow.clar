;; Fern Grow - Blockchain Ecosystem Simulation Game
;; A simple implementation of core game mechanics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-amount (err u104))

;; Data Variables
(define-data-var next-plot-id uint u1)
(define-data-var spore-per-health-point uint u10)

;; Data Maps
(define-map plots
  uint
  {
    owner: principal,
    biodiversity: uint,
    health: uint,
    last-care-block: uint,
    seed-variant: uint
  }
)

(define-map plot-neighbors
  uint
  (list 4 uint)
)

(define-map user-spore-balance
  principal
  uint
)

(define-map user-plot-count
  principal
  uint
)

;; Private Functions
(define-private (get-spore-balance (user principal))
  (default-to u0 (map-get? user-spore-balance user))
)

(define-private (calculate-ecosystem-reward (plot-id uint))
  (let (
    (plot-data (unwrap! (map-get? plots plot-id) u0))
    (health (get health plot-data))
    (biodiversity (get biodiversity plot-data))
  )
    (* (+ health biodiversity) (var-get spore-per-health-point))
  )
)

;; Public Functions

;; Mint a new plot NFT
(define-public (mint-plot (seed-variant uint))
  (let (
    (plot-id (var-get next-plot-id))
    (current-count (default-to u0 (map-get? user-plot-count tx-sender)))
  )
    (map-set plots plot-id {
      owner: tx-sender,
      biodiversity: u50,
      health: u50,
      last-care-block: block-height,
      seed-variant: seed-variant
    })
    (map-set user-plot-count tx-sender (+ current-count u1))
    (var-set next-plot-id (+ plot-id u1))
    (ok plot-id)
  )
)

;; Care for your plot (watering, pest management)
(define-public (care-for-plot (plot-id uint))
  (let (
    (plot-data (unwrap! (map-get? plots plot-id) err-not-found))
    (blocks-since-care (- block-height (get last-care-block plot-data)))
  )
    (asserts! (is-eq tx-sender (get owner plot-data)) err-unauthorized)
    
    ;; Update plot health based on care
    (map-set plots plot-id (merge plot-data {
      health: (if (< (get health plot-data) u100)
                  (+ (get health plot-data) u5)
                  u100),
      last-care-block: block-height
    }))
    
    ;; Reward SPORE tokens
    (let ((reward (calculate-ecosystem-reward plot-id)))
      (map-set user-spore-balance tx-sender 
        (+ (get-spore-balance tx-sender) reward))
      (ok reward)
    )
  )
)

;; Establish neighboring plots for cross-pollination
(define-public (set-neighbors (plot-id uint) (neighbors (list 4 uint)))
  (let ((plot-data (unwrap! (map-get? plots plot-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner plot-data)) err-unauthorized)
    (map-set plot-neighbors plot-id neighbors)
    (ok true)
  )
)

;; Cross-pollination with neighboring plots
(define-public (cross-pollinate (plot-id uint) (neighbor-id uint))
  (let (
    (plot-data (unwrap! (map-get? plots plot-id) err-not-found))
    (neighbor-data (unwrap! (map-get? plots neighbor-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get owner plot-data)) err-unauthorized)
    
    ;; Increase biodiversity through cross-pollination
    (map-set plots plot-id (merge plot-data {
      biodiversity: (if (< (get biodiversity plot-data) u100)
                       (+ (get biodiversity plot-data) u3)
                       u100)
    }))
    
    ;; Reward both plot owners
    (map-set user-spore-balance tx-sender 
      (+ (get-spore-balance tx-sender) u20))
    (map-set user-spore-balance (get owner neighbor-data)
      (+ (get-spore-balance (get owner neighbor-data)) u20))
    
    (ok true)
  )
)

;; Claim accumulated SPORE rewards
(define-public (claim-rewards)
  (let ((balance (get-spore-balance tx-sender)))
    (asserts! (> balance u0) err-invalid-amount)
    (map-set user-spore-balance tx-sender u0)
    (ok balance)
  )
)

;; Read-only Functions

(define-read-only (get-plot-data (plot-id uint))
  (map-get? plots plot-id)
)

(define-read-only (get-plot-neighbors (plot-id uint))
  (map-get? plot-neighbors plot-id)
)

(define-read-only (get-user-balance (user principal))
  (ok (get-spore-balance user))
)

(define-read-only (get-user-plots (user principal))
  (ok (default-to u0 (map-get? user-plot-count user)))
)

(define-read-only (get-ecosystem-health (plot-id uint))
  (match (map-get? plots plot-id)
    plot-data (ok {
      health: (get health plot-data),
      biodiversity: (get biodiversity plot-data),
      combined-score: (+ (get health plot-data) (get biodiversity plot-data))
    })
    err-not-found
  )
)