program crow_and_fox
use iso_fortran_env,              only : stdout => output_unit
use,intrinsic :: iso_fortran_env, only : int8,int16,int32,int64
use,intrinsic :: iso_fortran_env, only : byte=>int8
use M_unicode, only : unicode_type, ut=>unicode_type, character, len
implicit none
type(unicode_type),allocatable :: poem(:)
integer :: i
integer                     :: iostat

   ! preferred, but not required if not supported
   open(stdout,encoding='utf-8',iostat=iostat)

   ! “The Crow and the Fox” by Jean de la Fontaine
   write(stdout,'(a,/)') 'Le Corbeau et le Renard -- Jean de la Fontaine' 

   poem=[&
   ut( 'Le Corbeau et le Renard'                             ), &
   ut( ''                                                    ), &
   ut( 'Maître Corbeau, sur un arbre perché,'                ), &
   ut( 'Tenait en son bec un fromage.'                       ), &
   ut( 'Maître Renard, par l’odeur alléché,'                 ), &
   ut( 'Lui tint à peu près ce langage :'                    ), &
   ut( '«Hé ! bonjour, Monsieur du Corbeau.'                 ), &
   ut( 'Que vous êtes joli ! que vous me semblez beau !'     ), &
   ut( 'Sans mentir, si votre ramage'                        ), &
   ut( 'Se rapporte à votre plumage,'                        ), &
   ut( 'Vous êtes le Phénix des hôtes de ces bois.»'         ), &
   ut( 'A ces mots le Corbeau ne se sent pas de joie ;'      ), &
   ut( 'Et pour montrer sa belle voix,'                      ), &
   ut( 'Il ouvre un large bec, laisse tomber sa proie.'      ), &
   ut( 'Le Renard s’en saisit, et dit : «Mon bon Monsieur,'  ), &
   ut( 'Apprenez que tout flatteur'                          ), &
   ut( 'Vit aux dépens de celui qui l’écoute :'              ), &
   ut( 'Cette leçon vaut bien un fromage, sans doute.»'      ), &
   ut( 'Le Corbeau, honteux et confus,'                      ), &
   ut( 'Jura, mais un peu tard, qu’on ne l’y prendrait plus.'), &
   ut( ' -- Jean de la Fontaine')]

   write(stdout,'(a)')'LINES'
   write(stdout,'(g0)')(character(poem(i)),i=1,size(poem))

   write(stdout,'(a)')'BYTES'
   write(stdout,'(*(a))')(poem(i)%byte(),new_line('a'),i=1,size(poem))

   write(stdout,'(a)')'WITH LENGTH IN GLYPHS AND BYTES'
   do i=1,size(poem)
      write(stdout,'(1x,i4.4,1x,i4.4,1x,*(g0))')len(poem(i)),len(poem(i)%character()),character(poem(i))
   enddo

end program crow_and_fox
