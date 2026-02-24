;;; nwscript-tempel.el --- code templates for nwscript -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Version: version
;; Package-Requires: (emacs tempel)
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(defmacro nwscript-tempel-setup! (mode &rest templates)
  (declare (indent defun))
  (let ((var (intern (concat (symbol-name mode) "-templates"))))
    `(progn
       (defvar ,var nil
         ,(format "Templates for %s." (symbol-name mode)))
       (setq ,var ',templates)
       (add-hook ',(intern (concat (symbol-name mode) "-hook"))
                 (lambda ()
                   (add-hook 'tempel-template-sources ',var nil t))))))

(setq nwscript-spell-schools '("Conjuration" "Conjuration [calling]" "Conjuration [healing]" "Conjuration [summoning]" "Conjuration [teleportation]"
                               "Divination" "Divination [scrying]"
                               "Enchantment" "Enchantment [charm]" "Enchantment [compulsion]"
                               "Illusion" "Illusion [figment]" "Illusion [glamor]" "Illusion [pattern]" "Illusion [phantasm]" "Illusion [shadow]"
                               "Abjuration" "Abjuration [dispel]"
                               "Evocation" "Necromancy" "Transmutation")
      nwscript-spell-caster-classes '("Cleric / Favored Soul"
                                      "Wizard / Sorcerer"
                                      "Druid / Shaman"
                                      "Warlock"
                                      "Bard"
                                      "Paladin"
                                      "Ranger")
      nwscript-spell-ranges '("Personal"
                              "Touch"
                              "Short"
                              "Medium"
                              "Long")
      nwscript-spell-components '("Verbal" "Somatic")
      nwscript-spell-descriptors '("Acid" "Air" "Chaotic" "Cold" "Darkness" "Death" "Earth"
                                   "Electricity" "Evil" "Fear" "Fire" "Force" "Good"
                                   "Language Dependent" "Lawful" "Light" "Mind Affecting"
                                   "Sonic" "Water" "Psionic" "Cure" "Harm" "Summoning"
                                   "Compulsion" "Polymorph" "Disease")
      nwscript-spell-saves '("Will Partial"
                             "Reflex Partial"
                             "Fortitude Partial"
                             "Will Negates"
                             "Reflex Negates"
                             "Fortitude Negates")
      nwscript-spell-sr '("Yes" "No")
      nwscript-spell-targets '("One Allied Creature"
                               "One Hostile Creature"
                               "Small"
                               "Medium"
                               "Large"
                               "Huge"
                               "Small Cone"
                               "Medium cone"
                               "Large Cone"
                               "Huge Cone"))

(defun nwscript-read-option (option)
  (pcase option
    ('school (completing-read "School: " nwscript-spell-schools))
    ('descriptors (string-join (completing-read-multiple "Descriptors: " nwscript-spell-descriptors)
                               ", "))
    ('levels (string-join (completing-read-multiple "Caster level: " nwscript-spell-caster-classes)
                          ", "))
    ('innate (completing-read "Innate Level: " (mapcar #'number-to-string (number-sequence 0 9))))
    ('components (string-join (completing-read-multiple "Components: " nwscript-spell-components)
                              ", "))
    ('range (completing-read "Range: " nwscript-spell-ranges))
    ('target (completing-read "Area of Effect / Target: " nwscript-spell-targets))
    ('save (string-join (completing-read-multiple "Save: " nwscript-spell-saves)
                        ", "))
    ('sr (completing-read "Spell Resistance: " nwscript-spell-sr))
    (_ (read-string "Misc:"))))

(defun nwscript-tempel-setup ()
  (nwscript-tempel-setup! nwscript-mode
    (spell "void mainSpell();"
           n> "\nvoid main()"
           n> "{"
           n> "AssignCommand(JXGetCaster(), mainSpell());" >
           n> "}" >
           n> "\nvoid mainSpell()"
           n> "{"
           n> "if (!X2PreSpellCastCode())"
           n> "{"
           n> "JXPostSpellCastCode();" >
           n> "return;" >
           n> "}" >
           n> "int iSpellId = JXGetSpellId();" >
           n> "int oCaster = OBJECT_SELF;" >
           n> "int iCasterLevel = JXGetCasterLevel(oCaster)" >
           n> q
           n> "JXPostSpellCastCode();" >
           n> "}" >)
    (spid "int iSpellId = JXGetSpellId();" >)
    (spt "object oTarget = JXGetSpellTargetObject();" >)
    (spl "location lLoc = JXGetSpellTargetLocation();" >)
    (sr "if (!MyResistSpell(" (p "oCaster" caster) ", " (p "oTarget" target) "))"
        n> "{"
        n> "SignalEvent(" (s target) ", EventSpellCastAt(" (s caster) ", JXGetSpellId(), " (p "IS_HARMFUL" is-harmful)"));" >
        n> q
        n> "}" >)
    (save "if (!MySavingThrow(" (p "SAVING_THROW_FORT" save) ", " (p "oTarget" target) ", " (p "JXGetSpellSaveDC()" dc) "))"
          n> "{"
          n> q
          n> "}" >)
    (spd n "/*"
         n "    "(read-string "Name: ")
         n> "Caster Level(s): " (nwscript-read-option 'levels)
         n> "Innate Level: " (nwscript-read-option 'innate)
         n> "School: "(nwscript-read-option 'school)
         n> "Descriptors: " (nwscript-read-option 'descriptors)
         n> "Components: " (nwscript-read-option 'components)
         n> "Range: " (nwscript-read-option 'range)
         n> "Target: " (nwscript-read-option 'target)
         n> "Save: " (nwscript-read-option 'save)
         n> "Spell Resistance: " (nwscript-read-option 'sr)
         n> "*Description*"
         "\n*/")
    (fee "effect " (p "eEffect" effect) " = GetFirstEffect(" (p "oTarget" target) ");"
         n> "while (GetIsEffectValid(" (s effect) "))"
         n> "{"
         n> "if (GetIsEffectValid(" (s effect) "))"
         n> "{"
         n> q
         n> "}" >
         n> (s effect) " = GetNextEffect(" (s target) ");" >
         n> "}" >)
    (aoe "object " (p "oTarget" target) " = GetFirstObjectInShape(" (p "SHAPE_SPHERE" shape) ", " (p "RADIUS_SIZE_LARGE" size) ", " (p "lLoc" location) ");"
         n> "while (GetIsObjectValid(" (s target)"))"
         n> "{"
         n> "if (spellsIsTarget(" (s target) ", " (p "SPELL_TARGET_SELECTIVEHOSTILE" type) ", " (p "OBJECT_SELF" caster) "))"
         n> "{"
         n> q
         n> "}" >
         n (s target) " = GetNextObjectInShape(" (s shape) ", " (s size) ", " (s location) ");" >
         n> "}" >)
    (struct "struct " p n> "{" n> q n> "};" >)
    (else "else if " p n> "{" n> q n> "}" >)
    (ife "if " p
         n> "{"
         n> p n> "}" >
         n> "else " p
         n> "{" >
         n> q
         n> "}" >)
    (if "if " p n> "{" n> q n> "}" >)
    (switch "switch " p n> "{"
            n> "case " p ": " q";" >
            n> "}" >)
    (while "while (" p ")"
           n> "{"
           n> q
           n> "}" >)
    (for "for (" p "; " p "; " p ")"
         n> "{"
         n> q
         n> "}" >)
    (main "void main(" p ")" n> "{" n> q n> "}" >)))


(provide 'nwscript-tempel)
;;; nwscript-tempel.el ends here
