
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
   *	MODULE:		dyn_consts.h
   *	DESCRIPTION:	DYN constants' definitions
   *
   * The contents of this file are subject to the Interbase Public
   * License Version 1.0 (the "License"); you may not use this file
   * except in compliance with the License. You may obtain a copy
   * of the License at http://www.Inprise.com/IPL.html
   *
   * Software distributed under the License is distributed on an
   * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
   * or implied. See the License for the specific language governing
   * rights and limitations under the License.
   *
   * The Original Code was created by Inprise Corporation
   * and its predecessors. Portions created by Inprise Corporation are
   * Copyright (C) Inprise Corporation.
   *
   * All Rights Reserved.
   * Contributor(s): ______________________________________.
   *
   * 18.08.2006 Dimitry Sibiryakov: Extracted it from ibase.h
   *
    }
{$ifndef INCLUDE_DYN_CONSTS_H}
{$define INCLUDE_DYN_CONSTS_H}  
  {******************************************** }
  { Dynamic Data Definition Language operators  }
  {******************************************** }
  {**************** }
  { Version number  }
  {**************** }

  const
    isc_dyn_version_1 = 1;    
    isc_dyn_eoc = 255;    
  {**************************** }
  { Operations (may be nested)  }
  {**************************** }
    isc_dyn_begin = 2;    
    isc_dyn_end = 3;    
    isc_dyn_schema_name = 4;    
    isc_dyn_def_database = 5;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_def_global_fld = 6;    
    isc_dyn_def_local_fld = 7;    
    isc_dyn_def_idx = 8;    
    isc_dyn_def_rel = 9;    
    isc_dyn_def_sql_fld = 10;    
    isc_dyn_def_view = 12;    
    isc_dyn_def_trigger = 15;    
    isc_dyn_def_security_class = 120;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_def_dimension = 140;    
    isc_dyn_def_generator = 24;    
    isc_dyn_def_function = 25;    
    isc_dyn_def_filter = 26;    
    isc_dyn_def_function_arg = 27;    
    isc_dyn_def_shadow = 34;    
    isc_dyn_def_trigger_msg = 17;    
    isc_dyn_def_file = 36;    
    isc_dyn_mod_database = 39;    
    isc_dyn_mod_rel = 11;    
    isc_dyn_mod_global_fld = 13;    
    isc_dyn_mod_idx = 102;    
    isc_dyn_mod_local_fld = 14;    
    isc_dyn_mod_sql_fld = 216;    
    isc_dyn_mod_view = 16;    
    isc_dyn_mod_security_class = 122;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_mod_trigger = 113;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_mod_trigger_msg = 28;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_database = 18;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_rel = 19;    
    isc_dyn_delete_global_fld = 20;    
    isc_dyn_delete_local_fld = 21;    
    isc_dyn_delete_idx = 22;    
    isc_dyn_delete_security_class = 123;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_dimensions = 143;    
    isc_dyn_delete_trigger = 23;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_trigger_msg = 29;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_filter = 32;    
    isc_dyn_delete_function = 33;    
    isc_dyn_delete_shadow = 35;    
    isc_dyn_grant = 30;    
    isc_dyn_revoke = 31;    
    isc_dyn_revoke_all = 246;    
    isc_dyn_def_primary_key = 37;    
    isc_dyn_def_foreign_key = 38;    
    isc_dyn_def_unique = 40;    
    isc_dyn_def_procedure = 164;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_procedure = 165;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_def_parameter = 135;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_delete_parameter = 136;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_mod_procedure = 175;    { only used in pretty.cpp; nobody generates it }
  { Deprecated.
  #define isc_dyn_def_log_file              176
  #define isc_dyn_def_cache_file            180
   }
    isc_dyn_def_exception = 181;    
    isc_dyn_mod_exception = 182;    
    isc_dyn_del_exception = 183;    
  { Deprecated.
  #define isc_dyn_drop_log                  194
  #define isc_dyn_drop_cache                195
  #define isc_dyn_def_default_log           202
   }
    isc_dyn_def_difference = 220;    
    isc_dyn_drop_difference = 221;    
    isc_dyn_begin_backup = 222;    
    isc_dyn_end_backup = 223;    
    isc_dyn_debug_info = 240;    
  {********************* }
  { View specific stuff  }
  {********************* }
    isc_dyn_view_blr = 43;    
    isc_dyn_view_source = 44;    
    isc_dyn_view_relation = 45;    
    isc_dyn_view_context = 46;    
    isc_dyn_view_context_name = 47;    
    isc_dyn_view_context_type = 48;    
  {******************** }
  { Generic attributes  }
  {******************** }
    isc_dyn_rel_name = 50;    
    isc_dyn_fld_name = 51;    
    isc_dyn_new_fld_name = 215;    
  {#define isc_dyn_idx_name                  52 }
    isc_dyn_description = 53;    
    isc_dyn_security_class = 54;    
    isc_dyn_system_flag = 55;    
    isc_dyn_update_flag = 56;    
    isc_dyn_prc_name = 166;    
  {#define isc_dyn_prm_name                  137 }
    isc_dyn_sql_object = 196;    
    isc_dyn_fld_character_set_name = 174;    
    isc_dyn_pkg_name = 247;    
    isc_dyn_fun_name = 251;    
  {****************************** }
  { Relation specific attributes  }
  {****************************** }
  {#define isc_dyn_rel_dbkey_length          61 }
  {#define isc_dyn_rel_store_trig            62 }
  {#define isc_dyn_rel_modify_trig           63 }
  {#define isc_dyn_rel_erase_trig            64 }
  {#define isc_dyn_rel_store_trig_source     65 }
  {#define isc_dyn_rel_modify_trig_source    66 }
  {#define isc_dyn_rel_erase_trig_source     67 }
    isc_dyn_rel_ext_file = 68;    
    isc_dyn_rel_sql_protection = 69;    
    isc_dyn_rel_constraint = 162;    
    isc_dyn_delete_rel_constraint = 163;    
    isc_dyn_rel_temporary = 238;    
    isc_dyn_rel_temp_global_preserve = 1;    
    isc_dyn_rel_temp_global_delete = 2;    
  {********************************** }
  { Global field specific attributes  }
  {********************************** }
    isc_dyn_fld_type = 70;    
    isc_dyn_fld_length = 71;    
    isc_dyn_fld_scale = 72;    
    isc_dyn_fld_sub_type = 73;    
    isc_dyn_fld_segment_length = 74;    
    isc_dyn_fld_query_header = 75;    
    isc_dyn_fld_edit_string = 76;    
    isc_dyn_fld_validation_blr = 77;    
    isc_dyn_fld_validation_source = 78;    
    isc_dyn_fld_computed_blr = 79;    
    isc_dyn_fld_computed_source = 80;    
    isc_dyn_fld_missing_value = 81;    
    isc_dyn_fld_default_value = 82;    
    isc_dyn_fld_query_name = 83;    
    isc_dyn_fld_dimensions = 84;    
    isc_dyn_fld_not_null = 85;    
    isc_dyn_fld_precision = 86;    
    isc_dyn_fld_char_length = 172;    
    isc_dyn_fld_collation = 173;    
    isc_dyn_fld_default_source = 193;    
    isc_dyn_del_default = 197;    
    isc_dyn_del_validation = 198;    
    isc_dyn_single_validation = 199;    
    isc_dyn_fld_character_set = 203;    
    isc_dyn_del_computed = 242;    
    isc_dyn_def_engine = 250;    
  {********************************* }
  { Local field specific attributes  }
  {********************************* }
    isc_dyn_fld_source = 90;    
    isc_dyn_fld_base_fld = 91;    
    isc_dyn_fld_position = 92;    
    isc_dyn_fld_update_flag = 93;    
    isc_dyn_fld_identity = 253;    
  {*************************** }
  { Index specific attributes  }
  {*************************** }
    isc_dyn_idx_unique = 100;    
    isc_dyn_idx_inactive = 101;    
    isc_dyn_idx_type = 103;    
    isc_dyn_idx_foreign_key = 104;    
    isc_dyn_idx_ref_column = 105;    
    isc_dyn_idx_statistic = 204;    
  {***************************** }
  { Trigger specific attributes  }
  {***************************** }
    isc_dyn_trg_type = 110;    
    isc_dyn_trg_blr = 111;    
    isc_dyn_trg_source = 112;    
  {#define isc_dyn_trg_name                  114 }
    isc_dyn_trg_sequence = 115;    
    isc_dyn_trg_inactive = 116;    
  {#define isc_dyn_trg_msg_number            117 }
    isc_dyn_trg_msg = 118;    
  {************************************ }
  { Security Class specific attributes  }
  {************************************ }
    isc_dyn_scl_acl = 121;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_grant_user = 130;    
    isc_dyn_grant_user_explicit = 219;    
    isc_dyn_grant_proc = 186;    
    isc_dyn_grant_trig = 187;    
    isc_dyn_grant_view = 188;    
    isc_dyn_grant_options = 132;    
    isc_dyn_grant_user_group = 205;    
    isc_dyn_grant_role = 218;    
    isc_dyn_grant_grantor = 245;    
    isc_dyn_grant_package = 248;    
    isc_dyn_grant_func = 252;    
    isc_dyn_fld_null = 249;    
  {******************************** }
  { Dimension specific information  }
  {******************************** }
    isc_dyn_dim_lower = 141;    
    isc_dyn_dim_upper = 142;    
  {************************** }
  { File specific attributes  }
  {************************** }
  {#define isc_dyn_file_name                 125 }
    isc_dyn_file_start = 126;    
    isc_dyn_file_length = 127;    
  {#define isc_dyn_shadow_number             128 }
    isc_dyn_shadow_man_auto = 129;    
    isc_dyn_shadow_conditional = 130;    
  {****************************** }
  { Log file specific attributes  }
  {****************************** }
  { Deprecated.
  #define isc_dyn_log_file_sequence         177
  #define isc_dyn_log_file_partitions       178
  #define isc_dyn_log_file_serial           179
  #define isc_dyn_log_file_overflow         200
  #define isc_dyn_log_file_raw              201
   }
  {************************* }
  { Log specific attributes  }
  {************************* }
  { Deprecated.
  #define isc_dyn_log_group_commit_wait     189
  #define isc_dyn_log_buffer_size           190
  #define isc_dyn_log_check_point_length    191
  #define isc_dyn_log_num_of_buffers        192
   }
  {****************************** }
  { Function specific attributes  }
  {****************************** }
    isc_dyn_function_name = 145;    
  {#define isc_dyn_function_type             146 }
    isc_dyn_func_module_name = 147;    
    isc_dyn_func_entry_point = 148;    
    isc_dyn_func_return_argument = 149;    
  {#define isc_dyn_func_arg_position         150 }
    isc_dyn_func_mechanism = 151;    
    isc_dyn_filter_in_subtype = 152;    
    isc_dyn_filter_out_subtype = 153;    
  {#define isc_dyn_description2              154 }
  {#define isc_dyn_fld_computed_source2      155 }
  {#define isc_dyn_fld_edit_string2          156 }
  {#define isc_dyn_fld_query_header2         157 }
  {#define isc_dyn_fld_validation_source2    158 }
  {#define isc_dyn_trg_msg2                  159 }
  {#define isc_dyn_trg_source2               160 }
  {#define isc_dyn_view_source2              161 }
  {#define isc_dyn_xcp_msg2                  184 }
  {******************************* }
  { Generator specific attributes  }
  {******************************* }
  {#define isc_dyn_generator_name            95 }
  {#define isc_dyn_generator_id              96 }
  {******************************* }
  { Procedure specific attributes  }
  {******************************* }
  {#define isc_dyn_prc_inputs                167 }
  {#define isc_dyn_prc_outputs               168 }
    isc_dyn_prc_source = 169;    { only used in pretty.cpp; nobody generates it }
    isc_dyn_prc_blr = 170;    { only used in pretty.cpp; nobody generates it }
  {#define isc_dyn_prc_source2               171 }
  {#define isc_dyn_prc_type                  239 }
  {#define isc_dyn_prc_t_selectable          1 }
  {#define isc_dyn_prc_t_executable          2 }
  {******************************* }
  { Parameter specific attributes  }
  {******************************* }
  {#define isc_dyn_prm_number                138 }
  {#define isc_dyn_prm_type                  139 }
  {#define isc_dyn_prm_mechanism             241 }
  {****************************** }
  { Relation specific attributes  }
  {****************************** }
    isc_dyn_xcp_msg = 185;    
  {******************************************** }
  { Cascading referential integrity values      }
  {******************************************** }
    isc_dyn_foreign_key_update = 205;    
    isc_dyn_foreign_key_delete = 206;    
    isc_dyn_foreign_key_cascade = 207;    
    isc_dyn_foreign_key_default = 208;    
    isc_dyn_foreign_key_null = 209;    
    isc_dyn_foreign_key_none = 210;    
  {********************* }
  { SQL role values      }
  {********************* }
    isc_dyn_def_sql_role = 211;    
    isc_dyn_sql_role_name = 212;    
    isc_dyn_grant_admin_options = 213;    
    isc_dyn_del_sql_role = 214;    
  { 215 & 216 are used some lines above.  }
  {******************************************** }
  { Generators again                            }
  {******************************************** }
    isc_dyn_delete_generator = 217;    
  { New for comments in objects. }
    isc_dyn_mod_function = 224;    
  {#define isc_dyn_mod_filter                225 }
  {#define isc_dyn_mod_generator             226 }
  {#define isc_dyn_mod_sql_role              227 }
  {#define isc_dyn_mod_charset               228 }
  {#define isc_dyn_mod_collation             229 }
  {#define isc_dyn_mod_prc_parameter         230 }
  {********************* }
  { collation values     }
  {********************* }
    isc_dyn_def_collation = 231;    
    isc_dyn_coll_for_charset = 232;    
    isc_dyn_coll_from = 233;    
    isc_dyn_coll_from_external = 239;    
    isc_dyn_coll_attribute = 234;    
    isc_dyn_coll_specific_attributes_charset = 235;    
    isc_dyn_coll_specific_attributes = 236;    
    isc_dyn_del_collation = 237;    
  {**************************************** }
  { Mapping OS security objects to DB ones  }
  {**************************************** }
    isc_dyn_mapping = 243;    
    isc_dyn_map_role = 1;    
    isc_dyn_unmap_role = 2;    
  {#define isc_dyn_map_user							3 }
  {#define isc_dyn_unmap_user							4 }
    isc_dyn_automap_role = 5;    
    isc_dyn_autounmap_role = 6;    
  {****************** }
  { Users control     }
  {****************** }
    isc_dyn_user = 244;    
    isc_dyn_user_add = 1;    
    isc_dyn_user_mod = 2;    
    isc_dyn_user_del = 3;    
    isc_dyn_user_passwd = 4;    
    isc_dyn_user_first = 5;    
    isc_dyn_user_middle = 6;    
    isc_dyn_user_last = 7;    
    isc_dyn_user_admin = 8;    
    isc_user_end = 0;    
  {************************** }
  { Last $dyn value assigned  }
  {************************** }
  {#define isc_dyn_last_dyn_value            254 }
{$endif}
  { INCLUDE_DYN_CONSTS_H }

