REPORT zabapgit_performance_test.

CLASS lcl_dummy_progress DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_progress.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dummy_progress IMPLEMENTATION.
  METHOD zif_abapgit_progress~set_total.
  ENDMETHOD.

  METHOD zif_abapgit_progress~show.
  ENDMETHOD.

  METHOD zif_abapgit_progress~off.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_popup DEFINITION.
  PUBLIC SECTION.
    METHODS popup_get_from_free_selections
      IMPORTING
        iv_title      TYPE zcl_abapgit_free_sel_dialog=>ty_syst_title OPTIONAL
        iv_frame_text TYPE zcl_abapgit_free_sel_dialog=>ty_syst_title OPTIONAL
      CHANGING
        ct_fields     TYPE zcl_abapgit_free_sel_dialog=>ty_free_sel_field_tab
      RAISING
        zcx_abapgit_cancel
        zcx_abapgit_exception.

    METHODS popup_perf_test_parameters
      EXPORTING
        !et_object_type_filter   TYPE zif_abapgit_definitions=>ty_object_type_range
        !et_object_name_filter   TYPE zif_abapgit_definitions=>ty_object_name_range
      CHANGING
        !cv_package              TYPE devclass
        !cv_include_sub_packages TYPE abap_bool
        !cv_main_language_only   TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
ENDCLASS.

CLASS lcl_popup IMPLEMENTATION.

  METHOD popup_perf_test_parameters.
    DATA: lt_fields TYPE zcl_abapgit_free_sel_dialog=>ty_free_sel_field_tab.
    FIELD-SYMBOLS: <ls_field> TYPE zcl_abapgit_free_sel_dialog=>ty_free_sel_field.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name = 'PACKAGE'.
    <ls_field>-only_parameter = abap_true.
    <ls_field>-ddic_tabname = 'TADIR'.
    <ls_field>-ddic_fieldname = 'DEVCLASS'.
    <ls_field>-param_obligatory = abap_true.
    <ls_field>-value = cv_package.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name = 'PGMID'.
    <ls_field>-only_parameter = abap_true.
    <ls_field>-ddic_tabname = 'TADIR'.
    <ls_field>-ddic_fieldname = 'PGMID'.
    <ls_field>-value = 'R3TR'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name = 'OBJECT'.
    <ls_field>-ddic_tabname = 'TADIR'.
    <ls_field>-ddic_fieldname = 'OBJECT'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name = 'OBJ_NAME'.
    <ls_field>-ddic_tabname = 'TADIR'.
    <ls_field>-ddic_fieldname = 'OBJ_NAME'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name = 'INCLUDE_SUB_PACKAGES'.
    <ls_field>-only_parameter = abap_true.
    <ls_field>-ddic_tabname = 'TDEVC'.
    <ls_field>-ddic_fieldname = 'IS_ENHANCEABLE'.
    <ls_field>-text = 'Include subpackages'.
    <ls_field>-value = cv_include_sub_packages.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name = 'MAIN_LANG_ONLY'.
    <ls_field>-only_parameter = abap_true.
    <ls_field>-ddic_tabname = 'TVDIR'.
    <ls_field>-ddic_fieldname = 'FLAG'.
    <ls_field>-text = 'Main language only'.
    <ls_field>-value = cv_main_language_only.

    popup_get_from_free_selections(
      EXPORTING
        iv_title       = 'Serialization Performance Test Parameters'
        iv_frame_text  = 'Parameters'
      CHANGING
        ct_fields      = lt_fields ).

    LOOP AT lt_fields ASSIGNING <ls_field>.
      CASE <ls_field>-name.
        WHEN 'PACKAGE'.
          cv_package = <ls_field>-value.
        WHEN 'OBJECT'.
          et_object_type_filter = <ls_field>-value_range.
        WHEN 'OBJ_NAME'.
          et_object_name_filter = <ls_field>-value_range.
        WHEN 'INCLUDE_SUB_PACKAGES'.
          cv_include_sub_packages = boolc( <ls_field>-value IS NOT INITIAL ).
        WHEN 'MAIN_LANG_ONLY'.
          cv_main_language_only = boolc( <ls_field>-value IS NOT INITIAL ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_get_from_free_selections.
    DATA: lo_free_sel_dialog TYPE REF TO zcl_abapgit_free_sel_dialog.

    CREATE OBJECT lo_free_sel_dialog
      EXPORTING
        iv_title      = iv_title
        iv_frame_text = iv_frame_text.

    lo_free_sel_dialog->set_fields( CHANGING ct_fields = ct_fields ).
    lo_free_sel_dialog->show( ).
  ENDMETHOD.

ENDCLASS.

"! Performance test run
CLASS zcl_abapgit_performance_test DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_result,
        pgmid    TYPE pgmid,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
        devclass TYPE devclass,
        counter  TYPE i,
        runtime  TYPE i,
        seconds  TYPE p LENGTH 16 DECIMALS 6,
      END OF ty_result,
      ty_results TYPE STANDARD TABLE OF ty_result WITH KEY pgmid object obj_name.
    METHODS:
      constructor IMPORTING iv_package              TYPE devclass
                            iv_include_sub_packages TYPE abap_bool DEFAULT abap_true
                            iv_main_language_only   TYPE abap_bool DEFAULT abap_true,
      set_object_type_filter IMPORTING it_object_type_range TYPE zif_abapgit_definitions=>ty_object_type_range,
      set_object_name_filter IMPORTING it_object_name_range TYPE zif_abapgit_definitions=>ty_object_name_range,
      get_object_type_filter RETURNING VALUE(rt_object_type_range) TYPE zif_abapgit_definitions=>ty_object_type_range,
      get_object_name_filter RETURNING VALUE(rt_object_name_range) TYPE zif_abapgit_definitions=>ty_object_name_range,
      run_measurement RAISING zcx_abapgit_exception,
      get_result RETURNING VALUE(rt_result) TYPE ty_results.
  PROTECTED SECTION.
  PRIVATE SECTION.


    METHODS:
      select_tadir_entries RETURNING VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
                           RAISING   zcx_abapgit_exception.
    DATA:
      mv_package              TYPE devclass,
      mv_include_sub_packages TYPE abap_bool,
      mv_main_language_only   TYPE abap_bool,
      BEGIN OF ms_filter_parameters,
        object_type_range TYPE zif_abapgit_definitions=>ty_object_type_range,
        object_name_range TYPE zif_abapgit_definitions=>ty_object_name_range,
      END OF ms_filter_parameters,
      mt_result TYPE ty_results.
ENDCLASS.



CLASS zcl_abapgit_performance_test IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] IV_INCLUDE_SUB_PACKAGES        TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_MAIN_LANGUAGE_ONLY          TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mv_package = iv_package.
    mv_include_sub_packages = iv_include_sub_packages.
    mv_main_language_only = iv_main_language_only.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->GET_OBJECT_NAME_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_OBJECT_NAME_RANGE           TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_OBJECT_NAME_RANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_object_name_filter.
    rt_object_name_range = ms_filter_parameters-object_name_range.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->GET_OBJECT_TYPE_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_OBJECT_TYPE_RANGE           TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_OBJECT_TYPE_RANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_object_type_filter.
    rt_object_type_range = ms_filter_parameters-object_type_range.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_RESULT                      TYPE        TY_RESULTS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_result.
    rt_result = mt_result.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->RUN_MEASUREMENT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run_measurement.
    DATA: li_actual_progress TYPE REF TO zif_abapgit_progress,
          lt_tadir           TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_tadir_single    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lo_serializer      TYPE REF TO zcl_abapgit_serialize,
          lv_start_runtime   TYPE i,
          lv_end_runtime     TYPE i,
          lx_exception       TYPE REF TO zcx_abapgit_exception,
          lo_dummy_progress  TYPE REF TO lcl_dummy_progress.
    FIELD-SYMBOLS: <ls_tadir>  TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_result> TYPE ty_result.

    CLEAR mt_result.

    li_actual_progress = zcl_abapgit_progress=>get_instance( 1 ).
    CREATE OBJECT lo_dummy_progress.
    zcl_abapgit_progress=>set_instance( lo_dummy_progress ).

    TRY.
        lt_tadir = select_tadir_entries( ).

        CREATE OBJECT lo_serializer
          EXPORTING
            iv_serialize_master_lang_only = mv_main_language_only.

        LOOP AT lt_tadir ASSIGNING <ls_tadir>.
          INSERT <ls_tadir> INTO TABLE lt_tadir_single.

          GET RUN TIME FIELD lv_start_runtime.

          lo_serializer->serialize(
            it_tadir            = lt_tadir_single
            iv_force_sequential = abap_true ).

          GET RUN TIME FIELD lv_end_runtime.

          APPEND INITIAL LINE TO mt_result ASSIGNING <ls_result>.
          <ls_result>-pgmid = <ls_tadir>-pgmid.
          <ls_result>-object = <ls_tadir>-object.
          <ls_result>-obj_name = <ls_tadir>-obj_name.
          <ls_result>-devclass = <ls_tadir>-devclass.
          <ls_result>-runtime = lv_end_runtime - lv_start_runtime.
          <ls_result>-seconds = <ls_result>-runtime / 1000000.

          CLEAR lt_tadir_single.
        ENDLOOP.

      CATCH zcx_abapgit_exception INTO lx_exception.
        zcl_abapgit_progress=>set_instance( li_actual_progress ).
        RAISE EXCEPTION lx_exception.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_PERFORMANCE_TEST->SELECT_TADIR_ENTRIES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_TADIR                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR_TT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select_tadir_entries.
    rt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = mv_package
      iv_ignore_subpackages = boolc( mv_include_sub_packages = abap_false ) ).

    DELETE rt_tadir WHERE object NOT IN ms_filter_parameters-object_type_range
                       OR obj_name NOT IN ms_filter_parameters-object_name_range.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->SET_OBJECT_NAME_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OBJECT_NAME_RANGE           TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_OBJECT_NAME_RANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_object_name_filter.
    ms_filter_parameters-object_name_range = it_object_name_range.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_PERFORMANCE_TEST->SET_OBJECT_TYPE_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OBJECT_TYPE_RANGE           TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_OBJECT_TYPE_RANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_object_type_filter.
    ms_filter_parameters-object_type_range = it_object_type_range.
  ENDMETHOD.
ENDCLASS.

FORM run RAISING zcx_abapgit_exception.
  DATA: lo_performance          TYPE REF TO zcl_abapgit_performance_test,
        lv_package              TYPE devclass,
        lv_include_sub_packages TYPE abap_bool VALUE abap_true,
        lv_main_language_only   TYPE abap_bool VALUE abap_true,
        lt_object_type_filter   TYPE zif_abapgit_definitions=>ty_object_type_range,
        lt_object_name_filter   TYPE zif_abapgit_definitions=>ty_object_name_range,
        lt_result               TYPE zcl_abapgit_performance_test=>ty_results,
        lo_alv                  TYPE REF TO cl_salv_table,
        lx_salv_error           TYPE REF TO cx_salv_error,
        lo_runtime_column       TYPE REF TO cl_salv_column,
        lo_seconds_column       TYPE REF TO cl_salv_column,
        li_popups               TYPE REF TO zif_abapgit_popups.


  NEW lcl_popup( )->popup_perf_test_parameters(
    IMPORTING
      et_object_type_filter   = lt_object_type_filter
      et_object_name_filter   = lt_object_name_filter
    CHANGING
      cv_package              = lv_package
      cv_include_sub_packages = lv_include_sub_packages
      cv_main_language_only   = lv_main_language_only ).

  CREATE OBJECT lo_performance
    EXPORTING
      iv_package              = lv_package
      iv_include_sub_packages = lv_include_sub_packages
      iv_main_language_only   = lv_main_language_only.


  lo_performance->set_object_type_filter( lt_object_type_filter ).
  lo_performance->set_object_name_filter( lt_object_name_filter ).

  lo_performance->run_measurement( ).

  lt_result = lo_performance->get_result( ).

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_result ).
      lo_alv->get_functions( )->set_all( ).
      lo_alv->get_display_settings( )->set_list_header( 'Serialization Performance Test Results' ).
      lo_runtime_column = lo_alv->get_columns( )->get_column( 'RUNTIME' ).
      lo_runtime_column->set_medium_text( 'Runtime' ).
      lo_runtime_column->set_visible( abap_false ).
      lo_seconds_column = lo_alv->get_columns( )->get_column( 'SECONDS' ).
      lo_seconds_column->set_medium_text( 'Seconds' ).
      lo_alv->get_columns( )->set_count_column( 'COUNTER' ).
      lo_alv->get_aggregations( )->add_aggregation( lo_runtime_column->get_columnname( ) ).
      lo_alv->get_aggregations( )->add_aggregation( lo_seconds_column->get_columnname( ) ).
      lo_alv->set_screen_popup(
        start_column = 1
        end_column   = 180
        start_line   = 1
        end_line     = 25 ).
      lo_alv->display( ).
    CATCH cx_salv_error INTO lx_salv_error.
      zcx_abapgit_exception=>raise(
        iv_text     = lx_salv_error->get_text( )
        ix_previous = lx_salv_error ).
  ENDTRY.
ENDFORM.

FORM foo.
  DATA lx_error TYPE REF TO zcx_abapgit_exception.
  TRY.
      PERFORM run.
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.
