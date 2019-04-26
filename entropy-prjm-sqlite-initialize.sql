CREATE TABLE Prjs_Index(
    Prj_ID text,
    Prj_Name text,
    Prj_Author text,
    Prj_Date text,
    Prj_State
);

CREATE TABLE Prjs_Attr (
    Prj_ID text,
    Prj_Attr_Type text,
    Prj_Attr_Category text,
    Prj_Attr_Rfc text,
    Prj_Attr_Uri text,
    Prj_Attr_Des text
);

CREATE TABLE Prjs_Vcs (
    Prj_ID text,
    Prj_Vcs_Type text,
    Prj_Vcs_Remote text,
    Prj_Vcs_Status text,
    Prj_Vcs_Head text
);

CREATE TABLE DB_Version (
    Version text
);


INSERT INTO DB_Version (Version)
VALUES ('0.1.0');

INSERT INTO Prjs_Index (Prj_ID,Prj_Name,Prj_Author,Prj_Date,Prj_State)
VALUES ('00000000000000', 'entropy-prj-template',Null,Null,Null);

INSERT INTO Prjs_Attr (Prj_ID,Prj_Attr_Type,Prj_Attr_Category,Prj_Attr_Rfc,Prj_Attr_Uri,Prj_Attr_Des)
VALUES ('00000000000000',
        'foo',null,null,
        '~/.entropy-emacs-deps/elements/submodules/entropy-prj/prj-template',
        'entropy project default template. Used for user to quickly create new project.');

INSERT INTO Prjs_Vcs (Prj_ID,Prj_Vcs_Type,Prj_Vcs_Status,Prj_Vcs_Head)
VALUES ('00000000000000',null,null,null);

