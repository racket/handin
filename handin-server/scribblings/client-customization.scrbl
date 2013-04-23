#lang scribble/doc
@(require "common.rkt"
          scribble/bnf)

@title{Client Customization}

@itemize[
@item{Rename (or make a copy of) the @filepath{handin-client}
collection directory.  The new name should describe your class
uniquely.  For example, @filepath{uu-cs1410} is a good name for CS
1410 at the University of Utah.}

@item{Edit the first three definitions of @filepath{info.rkt} in your
  renamed client collection:
  @itemize[
  @item{For @racket[name], choose a name for the handin tool as it
    will appear in DrRacket's interface (e.g., the @onscreen{XXX} for
    the @onscreen{Manage XXX Handin Account...}  menu item).  Again,
    make the name specific to the course, in case a student installs
    multiple handin tools.  Do not use @onscreen{Handin} as the last
    part of the name, since @onscreen{Handin} is always added for
    button and menu names.}

  @item{Uncomment the definitions of @racket[tools],
    @racket[tool-names], and @racket[tool-icons].  (But leave the
    latter field's definition as @filepath{icon.png}.)}

  @item{For @racket[server:port], uncomment the line, and use the
    hostname and port where the server will be running to accept
    handin submissions.}]

  Optionally uncomment and edit the next two definitions,
  @racket[web-menu-name] and @racket[web-address], to add an item to
  the @onscreen{Help} menu that opens a (course-specific) web page.}

@item{Replace @filepath{icon.png} in your renamed directory with a new
  32x32 icon.  This icon is displayed on startup with DrRacket's
  splash screen, and it is included at half size on the
  @onscreen{Handin} button.  A school logo is typically useful, as it
  provides a recognizably local visual cue.  If students might use
  multiple installed handin tools, then make sure to vary the icon
  according to the course.}

@item{Replace @filepath{server-cert.pem} in your renamed directory
  with a server certificate.  The file @filepath{server-cert.pem} in
  @filepath{handin-client} collection is ok for testing, but the point
  of this certificate is to make handins secure, so you should
  generate a new (self-certifying) certificate and keep its key
  private.  (See @secref{server-setup}.)}

@item{To create an installable package, follow the process described
  in @secref[#:doc '(lib "pkg/scribblings/pkg.scrbl")
  "how-to-create"].  Note that your copy of the
  @filepath{handin-client} directory is a collection directory, so it
  will go inside another directory that represents your
  package. Usually, both directory layers are called @nonterm{name}
  for some @nonterm{name} of your choice.}

@item{To create an installable @filepath{.plt} file instead of a
  package, first arrange for your copy of the @filepath{handin-client}
  directory to be an installed collection.  You can do that via
  @exec{raco link} (see @secref["link" #:doc '(lib
  "scribblings/raco/raco.scrbl")]), by by making sure that the copy is
  in the same place the original client directory was (see
  @secref{wheres-the-collection}) or by specifying a value for the
  @envvar{PLTCOLLECTS} environment variable.  For example, if your
  customized collection directory is
  located within @filepath{/home/joe}, then you can prefix the
  command below with @commandline{PLTCOLLECTS=/home/joe:}
  (and don't forget the colon at the end of the @envvar{PLTCOLLECTS}
  value; it is important!)

  With your copy of @filepath{handin-client} called @exec{@nonterm{name}}
  in place as a collection, run
  @;
  @commandline{raco pack --collect --at-plt ++setup @nonterm{name} @nonterm{name}.plt @nonterm{name}}
  @;
  You can also add a
  @tt{--replace} flag to make the installation of the resulting file
  replace existing files (useful for creating an update package).

  Note that if you create an updated copy of the client package (that
  is, students already have an older version installed), then you should
  use the @DFlag{replace} to indicate that the package should replace
  existing files instead of throwing an error.}

@item{Distribute either your package @nonterm{name} or the
  @filepath{@nonterm{name}.plt} archive to students for installation
  into their copies of DrRacket.  The students need not have access to
  the DrRacket installation directory; the tool will be installed on
  the filesystem in the student's personal space.  If you want to
  install it once on a shared installation, use @exec{raco setup} with the
  @DFlag{all-users} flag.}

]
