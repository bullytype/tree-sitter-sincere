const decimal_digits_pattern = /([0-9][0-9_]*[0-9]|[0-9])/;
const precedences = {
    mathexpr: 2,
    tuple_t: 2,
    array_t: 2,
    map_t: 2,
    set_t: 2,
    
    call: 3,
    access: 2,

    struct: 2,
    enum: 2,
    const: 1,
};

const OP_PREC = {
    primary: 9,
    unary: 8,
    multiplicative: 7,
    additive: 6,
    comparative: 5,
    and: 4,
    or: 3,
    isas: 2,
    composite_literal: -1,
};

// no bitwise operators by design
// providing bitwise intrinsics at least
const functionalOperators = [ 
    '#bits.or', '#bits.and', '#bits.xor', 
    '#bits.lshift', '#bits.rshift' 
]
const multiplicativeOperators = [ '*', '/', '%' ];
const additiveOperators = [ '+', '-' ];
const comparativeOperators = [ '==', '!=', '<', '<=', '>', '>=', '<=>' ];
// const assignmentOperators = multiplicativeOperators.concat(additiveOperators).map(operator => operator + '=').concat('=');

const intrinsicOperators = [
    '#sizeof', '#typeof', '#addressof', 
    // '#byteorder.to_network', '#byteorder.to_host',
    // '#byteorder.to_be', '#byteorder.to_le',
]

module.exports = grammar({
    name: 'sincere',
  
    word: $ => $._name,

    extras: $ => [
        $.comment,
        /[\s\f\uFEFF\u2060\u200B]|\r?\n/
    ],

    inline: $ => [
        $.top_level_statement,
        $.top_level_control_statement,
        $.top_level_symbol_definition,
        $.expression_statement,
        $.statement,
        $.expression,
        $.literal,
        $.type_like,
        //$.identifier,
        //$.value_expression,
        //$.meta_expression,
        //$.sc_dot_path,
        //$.dot_path,
    ],
    
    supertypes: $ => [
        //$.statement,
        //$.expression,
        //$.unary_expression,
        //$.binary_expression,
        //$.literal,
        //$.type_like
    ],

    conflicts: $ => [

    ],

    rules: {
        source_file: $ => repeat($.top_level_statement),
        
        // intrinsic: $ => seq(token.immediate('#'), choice($.identifier, $.access, $.call)),

        top_level_statement: $ => choice(
            $.top_level_control_statement,
            $.top_level_symbol_definition,
        ),

        /// CONTROL ///

        top_level_control_statement: $ => choice(
            $.load_statement,
            $.import_statement,
            $.top_level_if_statement,
        ),

        top_level_block: $ => seq(
            '{',
            repeat(choice($.top_level_control_statement, $.statement)),
            '}'
        ),

        top_level_if_statement: $ => prec.right(seq(
            '#if',
            optional(seq(
                field('initializer', comma_separated_list1($.parameter)), 
                ';'
            )),
            field('condition', $.expression),
            field('consequence', $.top_level_block),
            optional(seq(
                'else',
                field('alternative', choice($.top_level_if_statement, $.top_level_block))
            ))
        )),

        sc_dot_path: $ => token(/\.?([A-z0-9_]+\.)*([A-z0-9_]+\.sc)/),
        dot_path: $ => token(/\.?([A-z0-9_]+\.)*([A-z0-9_]+)/),

        load_statement: $ => seq(
            '#load',
            field('path', $.sc_dot_path),
            ';'
        ),
        
        import_statement: $ => seq(
            '#import',  
            field('path', $.dot_path), 
            optional(seq(
                'as', 
                field('alias', $.identifier)
            )),
            ';'
        ),

        /// DEFINITIONS ///

        top_level_symbol_definition: $ => seq(
            field('doc', optional($.doc_string_literal)),
            choice(
                $.constant_definition,
                $.function_definition,
                //$.struct_definition,
                //$.enum_definition,
                //$.concept_definition,
            )
        ),

        // v := 42;
        variable_definition: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            field('name', $.identifier),
            optional(seq(
                ':',
                field('type', $.type_like)
            )),
            ':=', 
            field('value', $.expression),
            ';'
        ),
        
        // v: u32;
        variable_declaration: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            field('name', $.identifier),
            ':',
            field('type', $.type_like),
            ';'
        ),

        // c :: 42;
        constant_definition: $ => prec(precedences['const'], seq(
            optional( field('attr', $.attribute_attachment) ),
            field('name', $.identifier),
            optional(seq(
                ':',
                field('type', $.type_like),
            )),
            //choice(
                '::',
                //seq(':', field('type', $.expression), ':'),
            //),
            field('value', $.expression),
            ';'
        )),

        // p: t = v
        parameter: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            field('name', $.identifier),
            choice(
                seq(
                    ':',
                    field('type', $.type_like),
                    optional(seq(
                        choice(':=', '::'),
                        field('default_value', $.expression)
                    ))
                ),
                seq(
                    ':=',
                    field('default_value', $.expression)
                ),
                seq(
                    '::',
                    field('default_value', $.expression)
                )
            )
        ),

        // fn :: () -> T {}
        // fn :: () => {}
        function_definition: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            field('name', $.identifier),
            '::',
            '(',
            comma_separated_list($.parameter),
            ')',
            choice(
                seq(
                    '->', 
                    field('return_type', $.type_like)
                ),
                '=>'
            ),
            field('body', $.block)
        ),

        struct_definition: $ => //prec.right(precedences['struct'], 
            seq(
                optional( field('attr', $.attribute_attachment) ),
                field('name', $.identifier),
                '::',
                'struct',
                '{',
                repeat(choice(
                    $.variable_declaration,
                    $.variable_definition,
                    $.constant_definition, 
                )),
                '}',
                ';'
            ),
        //),

        struct_literal: $ => //prec.right(precedences['struct'], 
            seq(
                optional( field('attr', $.attribute_attachment) ),
                'struct',
                '{',
                repeat(choice(
                    $.variable_declaration,
                    $.variable_definition,
                    $.constant_definition, 
                )),
                '}',
            ),
        //),

        enum_value: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            field('name', $.identifier),
            ':',
            field('value', $.expression),
            ';'
        ),

        enum_definition: $ => //prec.right(precedences['enum'], 
            seq(
                optional( field('attr', $.attribute_attachment) ),
                field('name', $.identifier),
                '->',
                'enum',
                '{',
                repeat($.enum_value),
                '}'
            ),
        //),

        enum_literal: $ => //prec.right(precedences['enum'], 
            seq(
                optional( field('attr', $.attribute_attachment) ),
                'enum',
                '{',
                repeat($.enum_value),
                '}'
            ),
        //),

        union_literal: $ => //prec.right(precedences['enum'], 
            seq(
                optional( field('attr', $.attribute_attachment) ),
                'union',
                '{',
                    $.type_like,
                    repeat1(seq(
                        ',',
                        $.type_like,
                    )),
                '}'
            ),
        //),

        applied_concept: $ => seq(
            '#', 
            token(/[A-z_][A-z0-9_]+/),
            //optional(seq(
            //    '[',
            //    comma_separated_list1($.expression), 
            //    ']'
            //))
        ),

        concept_literal: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            'concept',
            '[', 
                field('typename', $.identifier),
                optional(seq(
                    ':',
                    field('concept', $.applied_concept)//$.identifier)
                )),
                optional(seq(
                    ',',
                    comma_separated_list1($.parameter)
                )),
            ']',
            field('body', $.block)
        ),

        attribute_expression: $ => choice(
            $.identifier,
            //$.dot_path,
            //$.call
        ),

        attribute_attachment: $ => choice(
            seq(
                token.immediate('@'), 
                $.identifier
            ), 
            seq(
                '@[',
                $.attribute_expression,
                repeat(seq(
                    ';',
                    $.attribute_expression,
                )), 
                ']'
            )
        ),

        attribute_operator: $ => seq(
            '@(',
            $.expression,
            ')'
        ),

        /// STATEMENTS ///

        statement: $ => choice(
            $.variable_declaration,
            $.variable_definition,
            $.constant_definition,
            
            $.empty_statement,
            $.return_statement,
            $.expression_statement,

            $.assignment,

            $.if_statement,
            $.while_statement,
            $.for_statement,
            $.foreach_statement,
            $.break_statement,
            $.continue_statement,
            
            $.defer_statement,
            $.optional_statement,
            $.expected_statement,

            $.expects_statement,
            $.ensures_statement,
            $.assert_statement,
            $.requires_statement,

            $.async_statement,
            $.await_statement,

            $.block
        ),

        block: $ => prec.right(seq(
            '{', repeat($.statement), '}'
        )),
        
        async_statement: $ => seq('async', $.expression),
        await_statement: $ => seq('await', $.expression),
        
        empty_statement: _ => ';',
        break_statement: _ => seq('break', ';'),
        continue_statement: _ => seq('continue', ';'),
        return_statement: $ => seq('return', $.expression, ';'),

        expression_statement: $ => seq($.expression, ';'),

        assignment: $ => seq(
            field('reference', choice($.identifier, $.reference_operator)),
            '=',
            field('value', $.expression),
            ';'
        ),

        if_statement: $ => prec.right(seq(
            'if',
            optional(seq(
                comma_separated_list1($.parameter),
                ';'
            )),
            field('condition', $.expression),
            field('consequence', $.block),
            optional(seq(
                'else',
                field('alternative', choice($.if_statement, $.block))
            ))
        )),

        while_statement: $ => seq(
            'while',
            optional(seq(
                comma_separated_list1($.parameter),
                ';'
            )),
            field('condition', $.expression),
            field('body', $.block),
            optional(seq(
                'else',
                field('alternative', $.block)
            ))
        ),

        for_statement: $ => seq(
            'for',
            optional(seq(
                comma_separated_list($.parameter),
                ';'
            )),
            field('condition', $.expression),
            optional(seq(
                ';',
                field('deferred_action', $.expression)
            )),
            field('body', $.block),
            optional(seq(
                'else',
                field('alternative', $.block)
            ))
        ),

        iterator: $ => seq(
            field('name', $.identifier),
            optional(seq(
                ':',
                field('type', $.type_like)
            ))
        ),

        foreach_statement: $ => seq(
            'foreach',
            field('iterator', comma_separated_list1($.iterator)),
            'in',
            field('range', $.expression),
            field('body', $.block)
        ),

        defer_statement: $ => seq(
            'defer',
            comma_separated_list1($.expression),
            ';'
        ),

        optional_statement: $ => seq(
            field('condition', $.expression),
            '??',
            field('alternative', choice(
                $.block,
                $.expression_statement
            )),
        ),
        
        expected_statement: $ => seq(
            field('expectation', $.expression),
            '!!',
            field('alternative', choice(
                $.block,
                $.expression_statement
            )),
        ),

        requires_statement: $ => seq(
            'requires',
            optional(seq(
                '(',
                comma_separated_list1($.parameter),
                ')', 
                '=>'
            )),
            choice(
                $.block,
                $.expression_statement
            )
        ),

        assert_statement: $ => seq(
            'assert',
            $.expression_statement
        ),

        expects_statement: $ => seq(
            'expects',
            $.expression_statement
        ),

        ensures_statement: $ => seq(
            'ensures',
            $.expression_statement
        ),

        /// EXPRESSIONS ///
        
        expression: $ => choice(
            $.identifier,
            $.literal,
            $.list,
            $.map,
            $.call,
            $.access,
            $.mathexpr,
            $.parenthesis_expression,
            $.type_like,
            
            // todo
            //$.reference_operator,

            $.unary_expression,
            $.binary_expression,
        ),

        // todo
        reference_operator: $ => seq(
            '&(',
            comma_separated_list1($.identifier),
            ')'
        ),

        parenthesis_expression: $ => prec.right( seq('(', $.expression, ')') ),

        argument: $ => seq(
            optional(seq(
                field('name', $.identifier),
                ':'
            )),
            field('value', $.expression)
        ),

        call: $ => prec(precedences['call'], seq(
            field('callable', choice(
                $.identifier,
                $.access,
            )),
            '(',
            comma_separated_list($.argument),
            ')'
        )),

        access: $ => prec.right(precedences['access'],
            seq(
                field('lhs', $.expression),
                '.',
                field('rhs', $.expression)
            ),
        ),

        mathexpr: $ => prec(precedences['mathexpr'], seq(
            'calc',
            optional(seq(
                '->',
                field('return_type', $.type_like)
            )),
            '{',
            token.immediate(/[A-z0-9_;,\\'"\.\+\-*\/%()^|&=!<> \n]+/),
            '}',
        )),

        list: $ => prec.right(seq(
            '$(', 
            comma_separated_list1($.expression), 
            ')'
        )),

        mapping: $ => seq(
            field('key', $.expression),
            ':',
            field('value', $.expression),
        ),

        map: $ => prec.right(seq(
            '$(',
            comma_separated_list1($.mapping),
            ')'
        )),
        
        /// OPERATIONS ///
        unary_expression: $ => prec(OP_PREC.unary, seq(
            field('operator', choice('+', '-', 'not')),
            field('operand', $.expression),
        )),
      
        binary_expression: $ => {
            const table = [
                [OP_PREC.multiplicative, choice(...multiplicativeOperators)],
                [OP_PREC.additive, choice(...additiveOperators)],
                [OP_PREC.comparative, choice(...comparativeOperators)],
                [OP_PREC.and, 'and'],
                [OP_PREC.or, 'or'],
                [OP_PREC.isas, choice('is', 'as')],
            ];
        
            return choice(...table.map(([precedence, operator]) =>
                prec.left(precedence, seq(
                    field('left', $.expression),
                    field('operator', operator),
                    field('right', $.expression),
                )),
            ));
        },

        /// TYPES ///
        signed_int_t: $ => token(choice(
            'i8',
            'i16',
            'i32',
            'i64',
            'i128'
        )),

        unsigned_int_t: $ => token(choice(
            'u8',
            'u16',
            'u32',
            'u64',
            'u128'
        )),

        float_t: _ => token(choice(
            'f32',
            'f64',
            'f128'
        )),

        bytes_t: _ => token('bytes'),
        string_t: _ => token('string'),

        none_t: _ => token('none'),
        type_t: _ => token('type'),
        bool_t: _ => token('bool'),
        rune_t: _ => token('rune'),
        code_t: _ => token('code'),

        _primitive: $ => choice(
            $.signed_int_t,
            $.unsigned_int_t,
            $.float_t,
            $.bytes_t,
            $.string_t,
            $.none_t,
            $.type_t,
            $.bool_t,
            $.rune_t,
            $.code_t
        ),

        type_like: $ => choice(
            $.identifier,
            $._primitive,
            //$.dot_path, 
            $.option,
            $.expectation,
            $.reference,
            $.prototype,
            $.tuple_t,
            $.array_t,
            $.map_t,
            //$.applied_concept,
            //$.expression
        ),

        tuple_t: $ => prec.right(precedences['tuple_t'], seq(
            '$[', 
                $.type_like,
                repeat1(seq(
                    ',',
                    $.type_like
                )), 
            ']'
        )),
        
        array_t: $ => prec.right(precedences['array_t'], seq(
            '$[',
                field('type', $.type_like),
                optional(seq(
                    ',',
                    field('size', $.integer_literal)
                )),
            ']'
        )),

        map_t: $ => prec.right(precedences['map_t'], seq(
            '$[',
                field('key', $.type_like),
                ':',
                field('value', $.type_like),
            ']'
        )),

        option: $ => prec.right( seq('?[', $.type_like, ']') ), // seq($.type_like, '?'),
        expectation: $ => prec.right( seq( '![', $.type_like, ']') ), // seq($.type_like, '!'),
        reference: $ => prec.right( seq('&[', $.type_like, ']') ), // seq($.type_like, '&'),
        
        prototype_parameter: $ => seq(
            optional(seq(
                field('name', $.identifier),
                ':'
            )),
            field('type', $.type_like)
        ), 
        
        // function (i32, string) -> f32
        prototype: $ => seq(
            choice(seq('function', '('), 'φ('), 
            comma_separated_list($.prototype_parameter),
            ')',
            optional(seq(
                '->',
                field('return_type', $.type_like),
            ))
        ),

        // lambda (s: string) => {}
        lambda: $ => seq(
            optional( field('attr', $.attribute_attachment) ),
            choice(seq('lambda', '('), 'λ('),
            comma_separated_list($.parameter),
            ')',
            choice(
                seq(
                    '->',
                    field('return_type', $.type_like)
                ),
                '=>',
            ),
            field('body', $.block)
        ),

        /// LITERALS ///

        literal: $ => choice(
            $.boolean_literal,
            $.integer_literal,
            $.float_literal,
            $.rune_literal,
            $.string_literal,
            $.raw_string_literal,
            $.code_literal,
            $.raw_code_literal,
            $.lambda,
            $.struct_literal,
            $.enum_literal,
            $.union_literal,
            $.concept_literal,
        ),

        boolean_literal: $ => choice(
            $.true,
            $.false
        ),

        true: _ => token('true'),
        false: _ => token('false'),

        integer_literal: $ => token(seq(
            choice(
                decimal_digits_pattern,
                (/0[xX][0-9a-fA-F_]*[0-9a-fA-F]+/), // Hex
            ),
            optional(/u8|u16|u32|u64|u128|i8|i16|i32|i64|i128/)
        )),

        float_literal: $ => {
            const suffix = /[fFdDmM]/;
            const exponent = /[eE][+-]?[0-9][0-9_]*/;
            return token(seq(
                choice(
                    seq(
                        decimal_digits_pattern,
                        '.',
                        decimal_digits_pattern,
                        optional(exponent),
                        optional(suffix)
                    ),
                    seq(
                        '.',
                        decimal_digits_pattern,
                        optional(exponent),
                        optional(suffix)
                    ),
                    seq(
                        decimal_digits_pattern,
                        exponent,
                        optional(suffix)
                    ),
                    seq(
                        decimal_digits_pattern,
                        suffix
                    )
                ),
                optional(/f32|f64|f128/)
            ))
        },

        rune_literal: $ => seq(
            '\'',
            choice(
                token.immediate(/[^'\\]/), 
                $.escape_sequence
            ),
            '\'',
        ),

        string_literal: $ => seq(
            '"',
            repeat(choice(
                $.string_literal_fragment,
                $.escape_sequence
            )),
            '"'
        ),
    
        string_literal_fragment: $ => token.immediate(prec(1, /[^"\\\n]+/)),
        
        escape_sequence: $ => token(choice(
            /\\x[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?/,
            /\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]/,
            /\\U[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]/,
            /\\[^xuU]/,
        )),
        
        
        raw_string_literal: $ => token(seq(
            /""["]+/,
            optional(/([^"]|("[^"])|(""[^"]))+/),
            /""["]+/
            )),
            
        code_literal_fragment: $ => token.immediate(prec(1, /[^`\\\n]+/)),
        
        code_literal: $ => seq(
            '`',
            /[A-z_]+/,
            repeat1(choice(
                $.code_literal_fragment,
                $.escape_sequence
            )),
            '`'
        ),
        
        raw_code_literal: $ => token(seq(
            /``[`]+/,
            /[A-z_]+/,
            /([^`]|(`[^`])|(``[^`]))+/,
            /``[`]+/
        )),

        doc_string_literal: $  => token(seq(
            /~~[~]+/,
            optional(/([^~]|(~[^~])|(~~[^~]))+/),
            /~~[~]+/
        )),
        
        comment: $ => token(choice(
            seq('//', /[^\n\r]*/)
        )),

        _name: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
        //keyword: $ => choice(),
        identifier: $ => $._name, //choice($._name, $.keyword),
    }
})

function comma_separated_list(rule) {
    return optional(comma_separated_list1(rule))
}

function comma_separated_list1(rule) {
    return seq(
        rule,
        repeat(seq(
            ',',
            rule
        ))
    )
}
