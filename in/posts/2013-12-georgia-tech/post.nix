{ html, html-tags, file-path, scss }:

{
  title = "Georgia Tech course history";
  date  = "2013 Dec";
  slug  = "georgia-tech";

  css = scss ./georgia-tech.scss;

  thumbnail = file-path ./coc.jpg;

  abstract = ''
    2005 — 2013
  '';

  body = let
    inherit (html-tags) h2 ul;
    semester = name: courses: [
      (h2 name)
      (ul courses)
    ];
    course = number: grade: name: html ''
      <li class="course">
        <span class="number">${number}</span>
        <span class="grade">${grade}</span>
        <span class="name">${name}</span>
      </li>
    '';
  in [
    (semester "Fall 2013" [
      (course "CS 6210" "A" "Advanced Operating Systems")
      (course "CS 6422" "A" "Database System Implementation")
    ])
    (semester "Spring 2013" [
      (course "CS 6238" "B" "Secure Computing Systems")
      (course "CS 7560" "A" "Theory of Cryptography")
    ])
    (semester "Fall 2012" [
      (course "CS 6260" "A" "Applied Cryptography")
      (course "CS 6491" "A" "Computer Graphics")
    ])
    (semester "Summer 2012" [
      (course "CS 4590" "A" "Computer Audio")
      (course "CSE 6740" "B" "Computational Data Analysis")
    ])
    (semester "Spring 2012" [
      (course "CS 8803" "A" "Mobile Applications and Services")
    ])
    (semester "Fall 2011" [
      (course "CS 6520" "B" "Computational Complexity")
    ])
    (semester "Spring 2011" [
      (course "CS 6241" "A" "Compiler Design")
    ])
    (semester "Fall 2010" [
      (course "CS 6550" "B" "Design and Analysis of Algorithms")
    ])
    (semester "Spring 2010" [
      (course "CS 3220" "B" "Processor Design")
      (course "EAS 1600" "B" "Intro Environmental Science")
      (course "LCC 3401" "B" "Technical Comm Practices")
    ])
    (semester "Fall 2009" [
      (course "CS 4290" "A" "Advanced Computer Org")
      (course "CS 4392" "A" "Programming Languages")
      (course "CS 4540" "A" "Advanced Algorithms")
      (course "MATH 4782" "D" "Quantum Information and Computation")
    ])
    (semester "Summer 2009" [
      (course "CS 1171" "S" "Computing in MATLAB")
      (course "CS 4235" "A" "Intro to Info Security")
      (course "CS 4911" "A" "Design Capstone Project")
      (course "PHYS 2212" "A" "Intro Physics II")
    ])
    (semester "Spring 2009" [
      (course "COOP 4000" "V" "Co-op Work Assignment")
    ])
    (semester "Fall 2008" [
      (course "CS 3210" "A" "Design of Operating Systems")
      (course "CS 3240" "A" "Languages and Computation")
      (course "ISYE 3770" "A" "Statistics and Applications")
      (course "MATH 2406" "A" "Abstract Vector Spaces")
      (course "MATH 4022" "A" "Intro to Graph Theory")
    ])
    (semester "Summer 2008" [
      (course "COOP 4000" "V" "Co-op Work Assignment")
    ])
    (semester "Spring 2008" [
      (course "CS 2340" "A" "Objects and Design")
      (course "CS 3300" "A" "Intro to Software Engineering")
      (course "CS 4400" "A" "Intro to Database Systems")
      (course "CS 4510" "A" "Automata and Complexity")
    ])
    (semester "Fall 2007" [
      (course "CS 2200" "A" "Systems and Networks")
      (course "COOP 2000" "V" "Co-op Work Assignment")
    ])
    (semester "Summer 2007" [
      (course "CS 3510" "A" "Design and Analysis of Algorithms")
      (course "CS 4001" "A" "Computing and Society")
      (course "SPAN 2813" "A" "Special Topics (Spanish)")
    ])
    (semester "Spring 2007" [
      (course "COOP 2001" "V" "Co-op Work Assignment")
    ])
    (semester "Fall 2006" [
      (course "CS 2110" "A" "Computer Organization and Programming")
      (course "MATH 3012" "A" "Applied Combinatorics")
      (course "PHYS 2211" "A" "Intro Physics I")
      (course "PSYC 1101" "A" "General Psychology")
      (course "SPAN 1101" "A" "Patterns of Spanish I")
    ])
    (semester "Spring 2006" [
      (course "CS 1050" "A" "Constructing Proofs")
      (course "CS 1322" "A" "Object Oriented Programming")
      (course "ECON 2100" "A" "Economics and Policy")
      (course "MATH 2605" "A" "Calc III for Computer Science")
    ])
    (semester "Fall 2005" [
      (course "CS 1100" "S" "Freshman Leap Seminar")
      (course "CS 1321" "A" "Intro to Computing")
      (course "ENGL 1102" "A" "English Composition II")
      (course "HPS 1040" "B" "Health")
      (course "MATH 1502" "B" "Calculus II")
    ])
  ];
}
