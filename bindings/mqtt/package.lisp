(defpackage #:chipi.binding.mqtt
  (:use #:cl)
  (:nicknames #:binding-mqtt)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:import-from #:alexandria #:removef #:assoc-value)
  (:export #:mqtt-init #:mqtt-shutdown #:mqtt-subscribe #:mqtt-unsubscribe
           #:mqtt-publish #:mqtt-publish-confirmed #:mqtt-health-snapshot
           #:mqtt-error #:mqtt-timeout #:mqtt-disconnected #:mqtt-publication-error
           #:mqtt-binding #:mqtt-binding-received-at
           #:mqtt-diagnostic-event #:mqtt-diagnostic-event-topic
           #:mqtt-diagnostic-event-reason
           ;; Transitional transport API for applications migrating existing clients.
           #:make-mqtt-input #:mqtt-input #:ensure-thread-running #:subscribe-on
           #:stop #:connected-p #:publish-and-wait #:publish-confirmed
           #:health-snapshot #:topic-match-p #:publish-to #:unsubscribe-from))
