(ns participation.core
  "A participation states that something (a participant) likes to participate in something else (the participation). It can be compared to the observer pattern, where the participation is the observable and the participant is the observer. However the participation pattern is a functional construct in contrast to the observer pattern. Unlike an observer the participant is a pure function and doesn't do any side effects. For that reason the participation invokes every registered participant and collects its result.")

(def ^{:private true}
     registrations (atom {}))

(defn- build-participation-name
  ([namespace name-symbol]
     (str namespace "/" name-symbol))
  ([name-symbol] (build-participation-name *ns* name-symbol)))

(defn participation [participation-name & parameters]
  (let [registration (get @registrations
                          participation-name)
        topic-fn (:topic-fn registration)
        topic (apply topic-fn parameters)
        participations (:participations registration)
        participants (map resolve (get participations topic []))
        default-participant (:default-participant registration)
        fallback-participant (:fallback-participant registration)
        participants (if (and (empty? participants)
                              fallback-participant)
                       [fallback-participant]
                       participants)
        participants (if default-participant
                       (conj participants default-participant)
                       participants)]
    (into [] participants)))

(defn defparticipation
  [name topic-fn]
  (let [participation-name (build-participation-name name)]
    (intern *ns* (symbol name)
            (vary-meta (partial participation participation-name)
                       assoc :participation participation-name))
    (swap! registrations assoc participation-name
           {:topic-fn topic-fn
            :participations {}})))

(defn set-fallback-participant [participation-name fallback-participant]
  (swap! registrations assoc-in [participation-name :fallback-participant]
         fallback-participant))

(defn set-default-participant [participation-name default-participant]
  (swap! registrations assoc-in [participation-name :default-participant]
         default-participant))

(defn register-participant
  [participation-name participant topic]
  (let [registration (get @registrations
                           participation-name)]
    (if (nil? registration)
      (throw (Exception. (str "A participation with the name '"
                              participation-name
                              "' has not been registered yet.")))
      (swap! registrations
             update-in
             [participation-name :participations topic]
             #(vec (distinct (conj % participant)))))))

(defn defparticipant
  [name participation topic participant]
  (let [participation-name (if (fn? participation)
                             (:participation (meta participation))
                             participation)
        participant-symbol (symbol (str *ns*) name)]
    (intern *ns* (symbol name) participant)
    (register-participant participation-name participant-symbol topic)))

(defn register-fn
  ([participation topic f]
     (let [{:keys [ns name]} (meta f)
           ns (str ns)
           name (str name)
           fn-symbol (symbol ns name)
           participation-name (if (string? participation)
                                participation
                                (:participation (meta participation)))]
       (register-participant
        participation-name
        fn-symbol
        topic))))
