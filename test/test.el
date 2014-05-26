(require 'coverlay)

(expectations

  (desc "detect source file line")
  (expect t
    (coverlay-source-filep "SF:/path/to/target.js"))
  (expect nil
    (coverlay-source-filep "DA:15,1"))
  (expect nil
    (coverlay-source-filep "end_of_record"))

)
