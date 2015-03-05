#lang lipics

@(require scriblib/autobib
          scriblib/footnote)

@(define-cite ~cite citet generate-bibliography #:style lipics-style)

@;; Example bibliography entry
@(define asd
   (make-bib #:title "A Scanner Darkly"
             #:author (authors "Phillip K. Dick")
             #:date "1977"
             #:location (book-location #:publisher "Doubleday")))

@(define androids
   (make-bib #:title "Do Androids Dream of Electric Sheep?"
             #:author (authors "Phillip K. Dick")
             #:date "1968"
             #:location (book-location #:publisher "Doubleday")))

@title{Test@note{This is a title footnote}}
@author[#:affil-no "1"]{Bob Arctor}
@author[#:affil-no "2"]{Bob Arctor, Jr.}
@affil[#:affil-no "1"]{ @; the blank lines are significant. yes, that's ugly
Some University

Some place, Some country

@tt|{bob@some.edu}|}

@affil[#:affil-no "1"]{
Other University

Other place, Some country

@tt|{bob@other.edu}|}

@author-running{B. Arctor and B. Arctor, Jr.}
@title-running{Still a Test}

@copyright{Bob Arctor and Bob Arctor, Jr.}
@subject-classification{Dummy classification -- please refer to @url{http://www.acm.org/about/class/ccs98-html}}
@keywords{Dummy keyword -- please provide 1--5 keywords}
@doi{10.4230/LIPIcs.xxx.yyy.p}
@volume-info["Billy Editor and Bill Editors" "2" "Conf title" "1" "1" "1"]

@; @abstract{
@; This is not a real abstract. It just plays one on TV.
@; }
@include-abstract{example-abstract.scrbl}

@section{Introduction}

Lorem ipsum dolor sit amet, consectetur adipiscing elit.@~cite[asd] Vestibulum id sagittis
justo, nec fermentum augue.@~cite[androids] Donec sem massa, porttitor sed lorem vitae, euismod
feugiat velit. Vestibulum cursus sodales mi, et laoreet purus sodales
in. Curabitur elementum volutpat velit, nec mollis justo. Mauris massa risus,
ornare vitae vulputate a, facilisis id sem. Pellentesque vestibulum sapien ac
purus tincidunt blandit.@note{This is a footnote} Proin sem lorem, suscipit ut neque egestas, vulputate
tristique lacus. Curabitur mi metus, aliquet vitae consequat eget, posuere ut
dui. Aliquam ac porttitor mi.

Ut ullamcorper arcu nec nisl gravida, in accumsan risus bibendum. Quisque
turpis mauris, efficitur in magna in, ultrices cursus elit. Fusce sit amet
luctus augue. Mauris elementum quis ipsum ac maximus. Integer pretium libero
justo, nec tempus leo rutrum sed. Quisque non ex id dolor pulvinar
eleifend. Nam id lacus posuere, semper quam in, mattis odio. Ut viverra nec
justo nec suscipit. Suspendisse eleifend, odio a lobortis convallis, ante est
efficitur urna, eget egestas nisi libero sit amet ante. Fusce varius non ipsum
in dapibus. Integer quis ultricies nibh. Praesent ut ante nunc. Mauris
efficitur libero sem, sed tempor risus posuere id. Phasellus facilisis pretium
nisi et aliquam.

@subsection{Bah}

Sed in finibus magna. Nunc ornare, lectus vitae fermentum lobortis, nisi odio
vestibulum neque, nec aliquet lorem sem eu diam. Nulla quis urna turpis. Mauris
blandit turpis justo, sed egestas augue efficitur vel. Proin cursus nulla
iaculis est dapibus, at semper libero egestas. Praesent nulla tellus, convallis
ut rutrum at, semper ut magna. Duis sit amet leo bibendum, posuere orci nec,
convallis purus.


@section{Second}

 In sem nunc, vulputate sed porta egestas, viverra vel purus. Etiam vestibulum turpis ac mollis molestie. Sed non nunc quis orci dictum viverra ut sit amet enim. Integer sollicitudin, felis id tristique ultrices, sapien neque cursus massa, vel suscipit est nisl sed metus. Vestibulum eget dolor vitae sapien ultricies efficitur vitae sed nulla. Praesent sem odio, convallis quis sodales id, interdum non orci. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Mauris eu odio fermentum, rutrum diam vitae, volutpat neque. Aenean non vehicula libero. Donec ipsum leo, rutrum vel accumsan non, vestibulum id urna. Maecenas porta a metus et suscipit. Phasellus ut tempus sapien, nec venenatis magna. Aenean sit amet leo vel nunc egestas dignissim. In ultricies sem mauris, eu aliquam nisi viverra nec. Quisque a facilisis ex. Fusce bibendum viverra suscipit.

@subsection{2.1}

Nunc ultrices enim eget fringilla rhoncus. Cras ullamcorper lacus at convallis
ornare. Mauris a purus molestie, malesuada elit sit amet, porta magna. In
maximus in orci ut dapibus. Nam eu blandit ante. Suspendisse ac rhoncus
felis. Vivamus eu dictum nunc. Donec pulvinar nulla ac tempus cursus. Cras
tempor molestie mi quis lobortis. Morbi laoreet malesuada gravida. Fusce in
mauris non ante rutrum euismod. Nunc elementum eu ipsum eleifend bibendum. Sed
tellus ante, malesuada at ante at, ultricies tristique ante. Donec ullamcorper
ante in justo sagittis consequat. Donec dictum, enim commodo tincidunt
efficitur, augue eros mattis augue, ut varius sapien ligula sit amet
nulla. Praesent vehicula facilisis nunc.

@subsection{2.2}

Pellentesque vestibulum justo a elit tempus pulvinar. Donec accumsan blandit
tortor, imperdiet rutrum purus posuere et. Proin non urna purus. Proin sit amet
nisl arcu. In massa mi, euismod eget velit vitae, lacinia eleifend
nisi. Phasellus elementum varius tortor, non pretium velit aliquet vel. Morbi a
aliquet lacus. Morbi suscipit diam eget metus commodo condimentum. Morbi
venenatis posuere iaculis. Mauris ac eros sed justo maximus varius. Aliquam
erat volutpat. Vivamus malesuada justo magna, sit amet ultrices ipsum blandit
eget. Vivamus condimentum ex ex, in hendrerit sem gravida et. Suspendisse sit
amet tortor justo. Morbi erat metus, facilisis lobortis rhoncus sit amet,
imperdiet porttitor nisl.

@paragraph{Eh}

Praesent a nisi nec odio iaculis gravida a nec dui. Vivamus sollicitudin
pulvinar felis, consectetur viverra nunc gravida sed. Praesent vulputate lectus
vitae diam imperdiet, sed fermentum enim ullamcorper. Morbi feugiat orci ut
auctor efficitur. Quisque dapibus sollicitudin eros eu accumsan. Praesent
dictum eget justo id faucibus. Sed iaculis interdum rhoncus. Donec eget ligula
quis urna ornare convallis tempor non metus. Praesent vehicula dui quis est
ornare blandit. Morbi congue fringilla est ac volutpat. Fusce tincidunt posuere
tortor nec aliquet. Vivamus auctor finibus massa, id vestibulum augue lobortis
vitae. Nulla porta purus sit amet leo dignissim, non maximus velit
viverra. Pellentesque luctus et orci nec tempus. Etiam dignissim enim
venenatis, iaculis est non, lacinia orci. Aenean varius tristique tellus nec
pretium.

@paragraph*{Eeh}

Donec vitae risus ut sem accumsan pharetra interdum vitae lectus. Vivamus
luctus eu tellus a placerat. Nulla pharetra ex nibh. Phasellus at sagittis
arcu, suscipit consectetur justo. Morbi lorem metus, aliquet et varius nec,
dictum non metus. Sed at luctus erat. Pellentesque risus nisl, gravida sed
semper eu, suscipit consectetur sapien. Curabitur vehicula euismod
elit. Praesent facilisis gravida turpis ut cursus. Aliquam non dui ut tortor
suscipit porta pulvinar in felis. Vestibulum tincidunt interdum
semper. Praesent molestie libero non mauris fermentum tincidunt.


@section{Last}

Proin accumsan, massa a ornare auctor, est eros egestas nibh, at tincidunt
massa turpis ac lacus. Donec at orci in tortor commodo imperdiet vitae et
sapien. Nunc euismod varius quam a iaculis. Aenean nulla massa, ultricies
viverra interdum sed, pellentesque id massa. Nullam vitae nisl mollis orci
vestibulum tempor sed non mi. Cras massa nibh, vehicula quis arcu consequat,
aliquam volutpat ipsum. Maecenas a nisi ac metus aliquam sollicitudin. Nam vel
lacus sed libero eleifend pharetra ut et dolor. Sed id ipsum sed dui dictum
venenatis. Praesent id tincidunt ipsum, vitae tincidunt ipsum. Morbi ultrices
sapien vel tortor rhoncus facilisis. Proin tempus est placerat diam ultrices
mattis. Vivamus vitae odio rhoncus, viverra tortor a, fermentum quam. Etiam
vehicula, neque sit amet fermentum volutpat, mauris leo cursus nunc, non
fringilla lorem tellus sed nisi. Maecenas auctor, mauris quis pellentesque
egestas, ex nulla sollicitudin dui, imperdiet dapibus arcu tellus sed leo.

Fusce ultrices erat id pretium hendrerit. Nulla quis fermentum nulla. Lorem
ipsum dolor sit amet, consectetur adipiscing elit. Proin eu diam nec nisi
semper tristique. Nunc varius ipsum a scelerisque ultricies. Sed accumsan diam
laoreet, bibendum risus eget, porta quam. Aliquam faucibus eleifend
commodo. Praesent dapibus faucibus rutrum. Phasellus ut congue purus. In cursus
est eu nisi efficitur ornare. Etiam sed elit turpis. Sed maximus pellentesque
turpis, sed molestie elit porttitor vel. In hac habitasse platea
dictumst. Vivamus eget blandit nisl, id cursus massa. Cras bibendum, justo
sollicitudin tincidunt commodo, ante turpis rhoncus eros, ut hendrerit leo
purus in nisl.

@(generate-bibliography)
