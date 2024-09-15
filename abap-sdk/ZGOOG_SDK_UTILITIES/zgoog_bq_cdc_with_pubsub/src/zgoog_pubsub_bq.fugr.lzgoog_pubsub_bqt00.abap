*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGOOG_PUBSUB_BQ.................................*
DATA:  BEGIN OF STATUS_ZGOOG_PUBSUB_BQ               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGOOG_PUBSUB_BQ               .
CONTROLS: TCTRL_ZGOOG_PUBSUB_BQ
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGOOG_PUBSUB_BQ               .
TABLES: ZGOOG_PUBSUB_BQ                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
