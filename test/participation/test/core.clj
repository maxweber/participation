(ns participation.test.core
  (:use participation.core
        [lazytest.describe
         :only [describe it given do-it using testing]]
        [lazytest.expect :only [expect]]))

(def event-name "event_name")

(defparticipation "event-handling"
  (fn [event] (:name event)))

(def participation-name "participation.test.core/event-handling")

(defparticipant "participant1" event-handling event-name
  (fn [aggregate event] (assoc aggregate :participate1 (:name event))))

(defparticipant "participant2" participation-name event-name
  (fn [aggregate event] (assoc aggregate :participate2 (:name event))))

(defn participant3 [aggregate event]
  (assoc aggregate :participate3 (:name event)))

(register-fn participation-name
             event-name participant3)

(defn participant4 [aggregate event]
  (assoc aggregate :participate4 (:name event)))

(register-fn event-handling
             event-name participant4)

(defn apply-event [event]
  (let [aggregate {}
        fns (event-handling event)
        result-aggregate (reduce #(%2 %1 event) aggregate fns)]
    result-aggregate))

(defn default-participant [aggregate event]
  (assoc aggregate :default (:name event)))

(defn fallback-participant [aggregate event]
  (assoc aggregate :fallback (:name event)))

(describe "Participation"
  (given [event {:name event-name}
          result-aggregate (apply-event event)]
    (do-it "should let express the intention that something should be participated in something with a given topic"
      (let [{:keys [participate1 participate2]} result-aggregate]
        (expect (= event-name participate1))
        (expect (= event-name participate2))))
    (testing register-fn
      (it "should be possible to register a normal fn (defn) as participant"
        (= event-name (:participate3 result-aggregate)))
      (it "should can resolve the participation fn to a participation name"
        (= event-name (:participate4 result-aggregate)))))
  (given [_ (set-fallback-participant participation-name fallback-participant)
          event-name "a_new_event"
          event {:name event-name}
          result-aggregate (apply-event event)]
    (it "should be possible to set a fallback participant for a topic, which is only invoked if no other participant is registered"
      (and
       (= event-name (:fallback result-aggregate))
       (= nil (:default result-aggregate)))))
  (given [_ (set-default-participant participation-name default-participant)
          event {:name event-name}
          result-aggregate (apply-event event)
          {:keys [participate1 participate2 default]} result-aggregate]
    (it "should be possible to set a default participant, which participates on every topic of a participation"
      (and (= event-name participate1 participate2 default)
           (= nil (:fallback result-aggregate)))))
  (given [_ (set-default-participant participation-name default-participant)
          some-new-event-name "some_new_event"
          event {:name some-new-event-name}
          result-aggregate (apply-event event)
          {:keys [default]} result-aggregate]
    (do-it "should use the default participant even if no other participants are registered (even additionally to a possible registered fallback-participant)"
      (expect (= some-new-event-name default))
      (expect (= some-new-event-name (:fallback result-aggregate))))))
