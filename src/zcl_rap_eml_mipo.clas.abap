CLASS zcl_rap_eml_mipo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rap_eml_mipo IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    CONSTANTS TravelID TYPE string VALUE '36DA0BD08BB6924D170009027659A94B' ##NO_TEXT.



*    " step 1 - READ
*    READ ENTITIES OF ZI_RAP_Travel_mipo
*      ENTITY travel
*        FROM VALUE #( ( TravelUUID = TravelID ) )
*      RESULT DATA(travels)
*      FAILED DATA(failed)
*      REPORTED DATA(reported).
*
*    out->write( travels ).
*    out->write( failed ).
*    out->write( reported ).

*    " step 2 - READ with Fields
*    READ ENTITIES OF ZI_RAP_Travel_mipo
*      ENTITY travel
*        FIELDS ( AgencyID CustomerID )
*      WITH VALUE #( ( TravelUUID = TravelID ) )
*      RESULT DATA(travels).
*    out->write( travels ).

*    " step 3 - READ with All Fields
*    READ ENTITIES OF ZI_RAP_Travel_mipo
*         ENTITY travel
*           ALL FIELDS
*         WITH VALUE #( ( TravelUUID = TravelID ) )
*         RESULT DATA(travels).
*    out->write( travels ).

*    " step 4 - READ By Association
*    READ ENTITIES OF ZI_RAP_Travel_mipo
*      ENTITY travel BY \_Booking
*        ALL FIELDS WITH VALUE #( ( TravelUUID = TravelID ) )
*      RESULT DATA(bookings).
*
*    out->write( bookings ).

*    " step 5 - Unsuccessful READ
*    READ ENTITIES OF ZI_RAP_Travel_mipo
*      ENTITY travel
*        ALL FIELDS WITH VALUE #( ( TravelUUID = '11111111111111111111111111111111' ) )
*      RESULT DATA(travels)
*      FAILED DATA(failed)
*      REPORTED DATA(reported).
*
*    out->write( travels ).
*    out->write( failed-travel[ 1 ]-%fail ).    " complex structures not supported by the console output
*    out->write( lines( reported-travel ) ).  " complex structures not supported by the console output

    " step 6 - MODIFY Update
    MODIFY ENTITIES OF ZI_RAP_Travel_mipo
      ENTITY travel
        UPDATE
          SET FIELDS WITH VALUE
            #( ( TravelUUID  = TravelID
                 Description = 'I like RAP@openSAP' ) )

     FAILED DATA(failed)
     REPORTED DATA(reported).

    " step 6b - Commit Entities
    COMMIT ENTITIES
      RESPONSE OF ZI_RAP_Travel_mipo
      FAILED     DATA(failed_commit)
      REPORTED   DATA(reported_commit).

    out->write( 'Update done' ).

  ENDMETHOD.

ENDCLASS.
