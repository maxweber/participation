(ns participation.core
  "A participation states that something (a participant) likes to participate in something else (the participation). It can be compared to the observer pattern, where the participation is the observable and the participant is the observer. But the participation pattern in contrast to the observer pattern is a functional construct. Unlike an observer the participant is a pure function and doesn't do any side effects. For that reason the participation invokes every registered participant and collects its result.")

(def ^{:private true}
     registrations (atom {}))

(defn- build-participation-name
  ([namespace name-symbol]
     (str namespace "/" name-symbol))
  ([name-symbol] (build-participation-name *ns* name-symbol)))

(defn participation [participation-name parameters]
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

(defmacro defparticipation
  [name parameters topic-fn]
  (let [participation-name (build-participation-name name)]
    (swap! registrations assoc participation-name
           {:topic-fn (eval topic-fn)
            :participations {}})
    `(defn ~name ~parameters
       (participation ~participation-name ~parameters))))

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
                              "' has not be registered yet.")))
      (let [participations (:participations registration)
            participations-for-topic (get participations
                                          topic
                                          [])
            participations (assoc participations
                             topic
                             (into []
                                   (distinct (conj participations-for-topic
                                              participant))))
            registration (assoc registration
                            :participations
                            participations)]
        (swap! registrations
               assoc participation-name registration)))))

(defmacro defparticipant
  [name participation topic participant]
  (let [participation-name
        (if (symbol? participation)
          (build-participation-name participation)
          participation)
        namespace (str *ns*)
        n (str name)]
    `(do
       ~(list 'def name participant)
       (register-participant ~participation-name (symbol ~namespace ~n) ~topic))))

(defn register-fn
  ([participation-name topic f]
     (let [{:keys [ns name]} (meta f)
           ns (str ns)
           name (str name)
           fn-symbol (symbol ns name)
           participation-name (if (string? participation-name)
                                participation-name
                                (if-let [{:keys [ns name]} (meta participation-name)]
                                  (build-participation-name ns name)
                                  (throw (java.lang.IllegalArgumentException.
                                          (str
                                           "Don't know how to handle participation-name: "
                                           participation-name)))))]
       (register-participant
        participation-name
        fn-symbol
        topic))))
