(ns aleph.http.multipart
  (:require
    [byte-streams :as bs])
  (:import
    [java.util
     Locale]
    [java.io
     File]
    [java.nio
     ByteBuffer Charset]
    [java.net
     URLConnection]
    [io.netty.util.internal
     ThreadLocalRandom]
    [io.netty.handler.codec.http.multipart
     ])
  (:require-clojure :as cc))

;; form data and files are encoded differently
(defn- mime-encode [val])
(defn- base64-encode [val])
(def encoders
  {:base64 base64-encode
   :quoted-printable mime-encode})

(defn boundary []
  (-> (ThreadLocalRandom/current) .nextLong Long/toHexString .toLowerCase))

(defn mime-type-descriptor
  [^String mime-type ^String encoding]
  (str
    (-> (or mime-type "application/octet-stream") .trim (.toLowerCase Locale/US))
    (when encoding
      (str ";charset=" encoding))))

(defn- make-part
  "[internal] Generates a part map of the appropriate format"
  [{:keys [name content mime-type charset transfer-encoding] :or {:transfer-encoding :quoted-printable}}]
  (let [mt (or mime-type
               (when (instance? File content)
                 (URLConnection/guessContentTypeFromName (.getName ^File content))))]
    {:name name :content (bs/to-byte-buffer content)
     :mime-type (mime-type-descriptor mt charset)
     :transfer-encoding transfer-encoding}))

(defn- part-headers [name mime-type transfer-encoding]
  (let [te (cc/name transfer-encoding)
        cd (str "content-disposition: form-data; name=\"" (mime-encode name) \newline)
        ct (str "content-type: " mime-type \newline)
        cte (str "content-transfer-encoding: " te "\n\n")
        lcd (.length cd)
        lct (.length ct)
        lcte (.length cte)
        size (+ lcd lct lcte)
        buf (ByteBuffer/allocate size)]
    (doto buf
      (.put 0 (bs/to-byte-buffer cd))
      (.put lcd (bs/to-byte-buffer ct))
      (.put (+ lcd lct) (bs/to-byte-buffer cte)))))

(defn- prepare-part
  "[internal] Generates the byte representation of a part for the bytebuffer"
  [{:keys [name content mime-type charset transfer-encoding] :as part}]
  ;; encode name, content`
  (let [encoder (get (encoders transfer-encoding) mime-encode)
        headers (part-headers name mime-type transfer-encoding)
        body (encoder content)
        header-len (.length headers)
        size (+ header-len (.length body))
        buf (ByteBuffer/allocate size)]
    (doto buf
      (.put 0 headers)
      (.put header-len body))))

(defn multipart-body [boundary parts]
  (let [b (bs/to-byte-buffer boundary)
        b-len (+ 2 (.length boundary))
        ps (map (comp make-part prepare-part) parts)
        boundaries-size (* (inc (count parts)) b-len)
        part-size (reduce (fn [acc p] (+ acc (.length p))) 0 ps)
        buf (ByteBuffer/allocate (+ boundaries-size part-size))]
    (.put buf 0 b)
    (reduce (fn [idx part]
              (let [p-len (.length part)]
                (.put buf idx part)
                (.put buf (+ idx part-len) b)
                (+ idx part-len b-len))) b-len ps)
    buf))
