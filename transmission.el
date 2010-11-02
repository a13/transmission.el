(require 'url)
(require 'json)
(require 'mm-decode)

(defgroup transmission nil "transmission.el options")

;; (defcustom tr-base-url "http://127.0.0.1:9091/transmission/rpc"
;;   "transmission base url"
;;   :type 'string
;;   :group 'transmission)
;; (defcustom transmission-username ""
;;   "transmission user name"
;;   :type 'string
;;   :group 'transmission)
;; (defcustom transmission-password ""
;;   "transmission password"
;;   :type 'string
;;   :group 'transmission)

(defcustom transmission-session nil
  "transmission session list"
  :type '(repeat
	  (cons :tag "Transmission remote session settings"
	      (string :tag "Session name")
	      (set :format "%v"
		   (cons :format "%v"
			 (const :format "" :url)
			 (string :tag "Transmission RPC URL"))
		   (cons :format "%v"
			 (const :format "" :auth)
			 (const :tag "Authentication required" t))
		   (cons :format "%v"
			 (const :format "" :username)
			 (string :tag "Username"))
		   (cons :format "%v"
			 (const :format "" :password)
			 (string :tag "Password")))))
  :group 'transmission)

(defun transmission-customize ()
  (interactive)
  (customize-group 'transmission))

(defmacro session-option (option)
  "Get assoc value for option"
  (cdr (assq option (cdr (car transmission-session)))))


(defun transmission-base-url (&optional number)
  "Get base url, maybe it would be better to let it when needed"
  (session-option :url))


(setq tr-session nil)

(defun transmission-get-session-id ()
  ""
  (url-retrieve (transmission-base-url) 'tr-parse-session-id))

(defun tr-parse-session-id (status)
  "Callback to extract Session Id from server response"
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    ;; It's ugly, but who cares?
    (and (re-search-forward "X-Transmission-Session-Id: ")
         (let ((session-id (buffer-substring (point)
                                             (progn (forward-line 1) (1- (point))))))
           (setq tr-session session-id))))
  ;; we don't need no current buffers 
  (kill-buffer (current-buffer)))

;; (transmission-get-session-id)

(defun auth-base64-encode (name password)
  (base64-encode-string (concat name ":" password)))

(defmacro transmission-request (method arguments tag)
  (when (not tr-session) (transmission-get-session-id))
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json") 
                                     ("X-Transmission-Session-Id" . ,tr-session)))
;;                                   ("Authorization" . ,(concat "Basic " (auth-base64-encode tr-name tr-pass)))))
        (url-request-data (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
;;    (message url-request-data)
    (url-retrieve (transmission-base-url) 'tr-parse-srv-response))
  (if obj 'obj nil))


(defun tr-parse-srv-response (status)
  (with-current-buffer (current-buffer)
    (with-current-buffer (mm-handle-buffer (mm-dissect-buffer t))
      (goto-char (point-min))
      (setq obj (json-read)))))

(transmission-request "torrent-get" (:fields ("id" "name")) 136666)

(provide 'transmission)
