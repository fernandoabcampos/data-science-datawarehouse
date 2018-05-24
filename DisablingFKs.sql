alter table d_variable disable constraint D_VARIABLE_FK_TIPO_VARIABLE;
truncate table d_tipo_variable;
alter table d_variable enable constraint D_VARIABLE_FK_TIPO_VARIABLE;

alter table F_REGISTRO disable constraint D_REGISTRO_FK_MUNICIPIO;
alter table F_REGISTRO disable constraint D_REGISTRO_FK_VARIABLE;
D_REGISTRO_FK_ESTACION
