program crow_and_fox
use M_unicode, only : unicode_type, ut=>unicode_type, character, len
! “The Crow and the Fox” by Jean de la Fontaine
type(unicode_type),allocatable :: poem(:)
integer :: i
poem=[&
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

   write(*,'(g0)')(character(poem(i)),i=1,size(poem))
   write(*,'(a)')
   write(*,'(*(a))')(poem(i)%codepoint(),new_line('a'),i=1,size(poem))
   write(*,'(a)')
   write(*,'(*(g0))')(character(poem(i)),len(poem(i)),' ',len(poem(i)%character()),new_line('a'),i=1,size(poem))

end program crow_and_fox
