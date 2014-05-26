(require 'coverlay)

(expectations

  (desc "detect source file line")
  (expect t
    (coverlay-source-filep "SF:/path/to/target.js"))
  (expect nil
    (coverlay-source-filep "DA:15,1"))
  (expect nil
    (coverlay-source-filep "end_of_record"))

  (desc "detect data line")
  (expect nil
    (coverlay-data-linep "SF:/path/to/target.js"))
  (expect t
    (coverlay-data-linep "DA:15,1"))
  (expect nil
    (coverlay-data-linep "end_of_record"))

  (desc "detect end of record")
  (expect nil
    (coverlay-end-of-recordp "SF:/path/to/target.js"))
  (expect nil
    (coverlay-end-of-recordp "DA:15,1"))
  (expect t
    (coverlay-end-of-recordp "end_of_record"))


)
