# Oop

This is an educational exploration of OOP, from the perspective of using functions and closures to express encapsulation, please do not use this in production!

## Usage

    * (let ((person1 (oop:create :name "NMunro" :age 2345 :to-string (lambda (self) (format nil "~A: ~A" (oop:get-property self :name) (oop:get-property self :age))))))
        (oop:call person1 :to-string))
    "NMunro: 2345"

## Installation

    git clone https://github.com/nmunro/cloop.git ~/quicklisp/local-projects/cloop
    sbcl --eval '(asdf:load-system :oop)'
