%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Thesis style to be used in conjunction with the companion %
% "thesis_template.tex".  It is intended to format a thesis %
% or dissertation in accordance with the current (2007)     %
% Thesis/Dissertation Guidelines from the Graduate School   %
% at Texas Tech University                                  %
% (http://www.depts.ttu.edu/gradschool/current/thd.php).    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Report any problems to Robert Byerly, Department of       %
% Mathematics and Statistics, Texas Tech University         %
% (robert.byerly@ttu.edu)                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%This has *not* yet been tested "under fire", i.e., on a    %
%dissertation that has actually been approved by the grad   %
%school.  If you use it, *please* report any formatting     %
%issues to robert.byerly@ttu.edu.                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\def\filename{ttuthes2007}
\def\fileversion{1.2}
\def\filedate{2007/6/23} 
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ttuthes2007}[\filedate\space\fileversion\space%
       LaTeX document class.]

\LoadClass[oneside,openany,12pt]{book}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Margin  settings -- Some may need to be adjusted.     %
% (Tried the geometry package, but it lacked           %
%  flexibility).                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setlength{\voffset}{0pt}
\setlength{\topmargin}{-0.1875in}
\setlength{\hoffset}{0.75in}
\setlength{\oddsidemargin}{0pt}
\setlength{\textwidth}{5.75in}
\setlength{\textheight}{8.1875in}
%END SETTINGS FOR MARGINS

%To get the double spacing or 1.5 spacing the
%grad school wants:
\RequirePackage{setspace}
\onehalfspacing  %(or use \onehalfspacing in the document preamble.)


%The grad school doesn't like right-justification:
% \raggedright
\setlength{\parindent}{0.5in}
\setlength{\parskip}{6pt}




%To get the header
\headheight15pt %Makes fancyhdr happy
\RequirePackage{fancyhdr}
\lhead{}\chead{}
%\rhead{} should be customized in the document preamble
\pagestyle{fancy}

%%%%%%%%%%%%%%%%%%%%%%%%%
%For the copyright page %
%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\copyrightpage}[2]{%
\thispagestyle{empty}
\vbox to \textheight{
\vfill
\hfil Copyright #1, #2\hfil
\vfill}
}

%Graduate school doesn't want a colon after
%"Figure n" in figure and table captions.
%How about a period?
% \long\def\@makecaption#1#2{%
%   \vskip\abovecaptionskip
%   \sbox\@tempboxa{#1. #2}%
%   \ifdim \wd\@tempboxa >\hsize
%     #1. #2\par
%   \else
%     \global \@minipagefalse
%     \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
%   \fi
%   \vskip\belowcaptionskip}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Some of the stuff from here on is modified from an earlier %
%style file for theses written by an unknown person.        %
%Thanks! whoever you are!                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\renewcommand{\frontmatter}{%
   \if@openright
      \cleardoublepage
   \else
      \clearpage
   \fi
   \@mainmatterfalse
   \pagenumbering{roman}            %% Front Matter roman numerals
   \setcounter{page}{2}             %% Start on the second page 
}

\def\@makechapterhead#1{%
  \vspace*{-20 \p@}%
  {\parindent \z@ \centering \normalfont
    \ifnum \c@secnumdepth >\m@ne
      \if@mainmatter
%          \MakeUppercase
		   \textbf
         \@chapapp\space \thechapter
        \par\nobreak
%        \vskip 20\p@
      \fi
    \fi
    \interlinepenalty\@M
     \normalfont{#1} \par\nobreak
    \vskip 20\p@
  }}

\def\@makeschapterhead#1{%
  \vspace*{-20 \p@}%
  {\parindent \z@ \centering
    \normalfont %originally \normalfont
    \interlinepenalty\@M
%     \MakeUppercase
     #1\par\nobreak
    \vskip 20\p@
  }}




 \renewcommand*\l@part{\@dottedtocline{0}{0em}{1.5em}}
 \renewcommand*\l@chapter{\@dottedtocline{0}{1.5em}{2.75em}}
 \renewcommand*\l@section{\@dottedtocline{1}{4em}{2.75em}}
 \renewcommand*\l@subsection{\@dottedtocline{2}{5.75em}{2.75em}}


\def\numberlinetwo#1{\advance\hangindent\@tempdima
  \hbox to\@tempdima{\hfil #1\enspace}}



\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \if@mainmatter
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
%                         \addcontentsline{toc}{chapter}%
%                         {\protect\numberlinetwo{\Roman{chapter}}#1}%
%                         \addcontentsline{toc}{chapter}{\arabic{chapter}. #1}%
                         \addcontentsline{toc}{chapter}%
%                         {\@chapapp\space\thechapter. #1}%
                         {\thechapter. #1}%
                       \else
                         \addcontentsline{toc}{chapter}{#1}%
                       \fi
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
%                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
%                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}

\renewcommand\contentsname{\textbf{Table of Contents}}
\renewcommand\listfigurename{\textbf{List of Figures}}
\renewcommand\listtablename{\textbf{List of Tables}}
\renewcommand{\bibname}{\textbf{References}}
 \renewcommand\indexname{Index}
% \renewcommand\figurename{Figure}
% \renewcommand\tablename{Table}
% \renewcommand\partname{PART}
% \renewcommand\chaptername{CHAPTER}
 \renewcommand\appendixname{Appendix}
% \renewcommand\today{}

\renewcommand\chapter{\if@openright\cleardoublepage\else\clearpage\fi
                     \global\@topnum\z@
                    \@afterindenttrue
                    \secdef\@chapter\@schapter}

\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {3.5ex \@plus 1ex \@minus .2ex}%
                                   {.3ex \@plus.2ex}%
                                   {\normalfont\normalsize
                                   }}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {3.25ex\@plus 1ex \@minus .2ex}%
                                     {.2ex \@plus .2ex}%
                                     {\normalfont\normalsize
                                     \raggedright\noindent}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {3.25ex\@plus 1ex \@minus .2ex}%
                                     {.2ex \@plus .2ex}%
                                     {\normalfont\normalsize
                                     \raggedright\noindent}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize}}

\renewcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename}%
    \addcontentsline{toc}{chapter}{\textbf{\listfigurename}}
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}%
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }

\renewcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename}%
    \addcontentsline{toc}{chapter}{\textbf{\listtablename}}
      \@mkboth{%
          \MakeUppercase\listtablename}%
         {\MakeUppercase\listtablename}%
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }

%%%%%%%%%%%%%%%%%%%%%%%%%
%For the bibliography %
%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{chicago}
\renewcommand{\refname}{\textbf{References}}
    \renewcommand\floatpagefraction{.9}
    \renewcommand\textfraction{.1} %originally .1
    \setcounter{secnumdepth}{3}
    \setcounter{tocdepth}{3}
\endinput

