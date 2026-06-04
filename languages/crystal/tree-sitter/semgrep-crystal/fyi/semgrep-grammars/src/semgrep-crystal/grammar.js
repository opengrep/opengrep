/*
  semgrep-crystal

  Extends the standard Crystal grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-crystal/grammar');

module.exports = grammar(base_grammar, {
  name: 'crystal',

  externals: ($, previous) => previous.concat([
    $.semgrep_ellipsis,
  ]),

  conflicts: ($, previous) => previous.concat([
    [$.yield],
    [$._expression, $.call],
    [$.call],
    [$.constant, $.macro_interpolated_constant_segment],
    [$.type_declaration, $.property_bare_declaration],
    [$.keyword_type_declaration, $.property_bare_declaration],
    [$.call, $.property_identifier],
  ]),

  rules: {
    semgrep_metavariable: $ => token(prec(1000, /\$[A-Z_][A-Z_0-9]*/)),

    deep_ellipsis: $ => seq('<...', $._expression, '...>'),

    _expression: ($, previous) => choice(
      $.semgrep_metavariable,
      $.semgrep_ellipsis,
      $.deep_ellipsis,
      previous,
    ),

    property_identifier: $ => alias('property', $.identifier),

    modifier_if: ($, previous) => prec(-1, previous),
    modifier_ensure: ($, previous) => prec(-1, previous),

    beginless_range: ($, previous) => {
      const range_op = field('operator', alias(choice('..', '...'), $.operator));
      const end = field('end', $._expression);

      return prec.left('range_operator', seq(
        range_op,
        optional($._end_of_range),
        optional(end),
      ));
    },

    _type: ($, previous) => choice(
      alias($.semgrep_metavariable, $.constant),
      previous,
    ),

    macro_interpolated_constant_segment: $ => seq(
      $._constant_segment,
      $.macro_expression,
    ),

    constant: ($, previous) => prec.right(seq(
      optional('::'),
      choice($._constant_segment, $.macro_interpolated_constant_segment),
      repeat(seq(
        '::',
        choice($._constant_segment, $.macro_interpolated_constant_segment),
      )),
    )),

    _dot_call: ($, previous) => {
      const receiver = field('receiver', $._expression);

      const method = field('method', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.constant,
        alias($.identifier_method_call, $.identifier),
        alias($._operator_token, $.operator),
        $.instance_var,
      ));

      return prec('dot_operator', seq(receiver, '.', method));
    },

    call: ($, previous) => {
      const receiver_call = choice(
        $._dot_call,
        field('method', alias($.identifier_method_call, $.identifier)),
        $._global_method,
      );
      const ambiguous_call = field('method', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
      ));
      const macro_expanded_call = field('method', alias($.macro_expression, $.identifier));
      const property_call = field('method', choice(
        alias('property', $.identifier),
        alias('property?', $.identifier),
      ));

      const argument_list = field('arguments', choice(
        alias($.argument_list_with_parens, $.argument_list),
        alias($.argument_list_no_parens, $.argument_list),
      ));

      const argument_list_with_block = field('arguments', choice(
        alias($.argument_list_with_parens_and_block, $.argument_list),
        alias($.argument_list_no_parens_with_block, $.argument_list),
      ));

      const brace_block = field('block', alias($.brace_block, $.block));
      const do_end_block = field('block', alias($.do_end_block, $.block));

      return choice(
        prec('no_block_call', seq(receiver_call, optional(argument_list))),
        prec('no_block_call', seq(ambiguous_call, argument_list)),
        prec('no_block_call',
          seq(macro_expanded_call, field('arguments', alias($.argument_list_with_parens, $.argument_list))),
        ),
        prec('no_block_call',
          seq(property_call, field('arguments', alias($.property_argument_list, $.argument_list))),
        ),
        prec('brace_block_call',
          seq(
            property_call,
            field('arguments', alias($.property_argument_list, $.argument_list)),
            brace_block,
          ),
        ),
        prec('no_block_call',
          seq(property_call, field('arguments', alias($.argument_list_with_parens, $.argument_list))),
        ),
        prec('brace_block_call',
          seq(
            property_call,
            field('arguments', alias($.argument_list_with_parens, $.argument_list)),
            brace_block,
          ),
        ),

        prec('brace_block_call',
          seq(receiver_call, optional(argument_list), brace_block),
        ),
        prec('brace_block_call',
          seq(ambiguous_call, optional(argument_list), brace_block),
        ),

        prec('do_end_block_call',
          seq(receiver_call, optional(argument_list), do_end_block),
        ),
        prec('do_end_block_call',
          seq(ambiguous_call, optional(argument_list), do_end_block),
        ),

        prec('ampersand_block_call',
          seq(receiver_call, argument_list_with_block),
        ),
        prec('ampersand_block_call',
          seq(ambiguous_call, argument_list_with_block),
        ),
      );
    },

    implicit_object_call_unchainable: ($, previous) => {
      const chained_receiver = field('receiver',
        alias($.implicit_object_call_chainable, $.implicit_object_call),
      );

      const method_name = field('method', choice(
        alias($.implicit_object_method_identifier, $.identifier),
        alias($.implicit_object_method_operator, $.operator),
      ));

      const argument_list = field('arguments',
        alias($.argument_list_no_parens, $.argument_list),
      );

      const argument_list_with_block = field('arguments',
        alias($.argument_list_no_parens_with_block, $.argument_list),
      );

      const assignment_argument = field('arguments',
        alias($.argument_list_no_parens, $.argument_list),
      );

      return seq(
        optional(chained_receiver),
        choice(
          seq(method_name, argument_list),
          seq(method_name, argument_list_with_block),
          seq(method_name, '=', assignment_argument),
        ),
      );
    },

    implicit_object_call_chainable: ($, previous) => {
      const chained_receiver = field('receiver',
        alias($.implicit_object_call_chainable, $.implicit_object_call),
      );

      const method_name = field('method', choice(
        alias($.implicit_object_method_identifier, $.identifier),
        alias($.implicit_object_method_operator, $.operator),
      ));

      const argument_list = field('arguments', choice(
        alias($.argument_list_with_parens, $.argument_list),
        alias($.argument_list_no_parens, $.argument_list),
      ));

      const argument_list_with_block = field('arguments',
        alias($.argument_list_with_parens_and_block, $.argument_list),
      );

      const brace_block = field('block', alias($.brace_block, $.block));
      const do_end_block = field('block', alias($.do_end_block, $.block));

      const assignment_argument = field('arguments',
        alias($.argument_list_with_parens, $.argument_list),
      );

      const index_arguments = field('arguments',
        alias($.bracket_argument_list, $.argument_list),
      );

      return seq(
        optional(chained_receiver),
        choice(
          prec.right(seq(method_name, optional(
            field('arguments', alias($.argument_list_with_parens, $.argument_list)),
          ))),
          seq(
            method_name,
            field('method', alias($._start_of_index_operator, $.operator)),
            index_arguments,
            choice(']', ']?'),
          ),
          seq(method_name, '=', assignment_argument),
          seq(method_name, optional(argument_list), brace_block),
          seq(method_name, optional(argument_list), do_end_block),
          seq(method_name, argument_list_with_block),
          alias($.implicit_object_ivar, $.instance_var),
          alias($.implicit_object_index_operator, $.index_call),
        ),
      );
    },

    module_def: ($, previous) => seq(
      'module',
      field('name', choice(
        alias($.semgrep_metavariable, $.constant),
        $.constant,
        $.generic_type,
      )),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      'end',
    ),

    class_def: ($, previous) => seq(
      optional('abstract'),
      'class',
      field('name', choice(
        alias($.semgrep_metavariable, $.constant),
        $.constant,
        $.generic_type,
      )),
      optional(seq(
        '<',
        field('superclass', choice(
          alias($.semgrep_metavariable, $.constant),
          $.constant,
          $.generic_instance_type,
        )),
      )),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      'end',
    ),

    struct_def: ($, previous) => seq(
      optional('abstract'),
      'struct',
      field('name', choice(
        alias($.semgrep_metavariable, $.constant),
        $.constant,
        $.generic_type,
      )),
      optional(seq(
        '<',
        field('superclass', choice(
          alias($.semgrep_metavariable, $.constant),
          $.constant,
          $.generic_instance_type,
        )),
      )),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      'end',
    ),

    _base_method_def: ($, previous) => {
      const klass = field('class', seq(
        choice(alias($.semgrep_metavariable, $.constant), $.constant, $.self),
        '.',
      ));
      const name = field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        alias($.identifier_method_call, $.identifier),
        alias($.identifier_assign, $.identifier),
        alias($._operator_token, $.operator),
        alias('`', $.operator),
      ));
      const params = seq('(', field('params', optional($.param_list)), ')');
      const return_type = field('type', seq($._type_field_separator, $._bare_type));
      const forall = field('forall', $.forall);

      return prec.right(seq(
        'def',
        optional(klass),
        name,
        optional(params),
        optional(return_type),
        optional(forall),
      ));
    },

    _macro_signature: ($, previous) => {
      const name = field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        alias($.identifier_method_call, $.identifier),
        alias($._operator_token, $.operator),
        alias('`', $.operator),
      ));
      const params = seq(
        '(',
        field('params', optional($.param_list)),
        ')',
      );

      return seq(
        'macro',
        name,
        choice(params, $._terminator),
      );
    },

    param_list: ($, previous) => {
      const param = choice($.param, $.splat_param, $.double_splat_param, $.semgrep_ellipsis);

      return choice(
        seq(
          param,
          repeat(seq(',', param)),
          optional(seq(',', optional($.block_param))),
        ),
        $.block_param,
      );
    },

    param: ($, previous) => {
      const extern_name = field('extern_name', $.identifier);
      const name = field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        alias($.identifier_method_call, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
      ));
      const type = field('type', seq($._type_field_separator, $._bare_type));
      const default_value = field('default', seq('=', $._expression));

      return seq(
        repeat($.annotation),
        optional(extern_name),
        name,
        optional(type),
        optional(default_value),
      );
    },

    splat_param: ($, previous) => {
      const name = field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
      ));
      const type = field('type', seq($._type_field_separator, $._bare_type));

      return seq(
        repeat($.annotation),
        '*',
        optional(name),
        optional(type),
      );
    },

    double_splat_param: ($, previous) => {
      const name = field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
      ));
      const type = field('type', seq($._type_field_separator, $._bare_type));

      return seq(
        repeat($.annotation),
        '**',
        name,
        optional(type),
      );
    },

    block_param: ($, previous) => {
      const name = field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
      ));
      const type = field('type', seq(/:\s/, $._bare_type));

      return seq(
        repeat($.annotation),
        '&',
        optional(name),
        optional(type),
      );
    },

    block_param_list: ($, previous) => {
      const param = choice(
        alias($.block_body_param, $.param),
        alias($.block_body_splat_param, $.splat_param),
        $.semgrep_ellipsis,
        $._block_body_nested_param,
      );

      return seq(
        param,
        repeat(seq(',', param)),
        optional(','),
      );
    },

    block_body_param: ($, previous) => {
      const name = field('name', choice(
        alias($.semgrep_metavariable, $.identifier),
        $.identifier,
        $.underscore,
      ));

      return name;
    },

    block_body_splat_param: ($, previous) => seq(
      '*',
      field('name', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.underscore,
      )),
    ),

    type_declaration: ($, previous) => {
      const variable = field('var', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        alias($.identifier_method_call, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
      ));
      const type = field('type', $._bare_type);
      const value = field('value', $._expression);

      return prec('assignment_operator', seq(
        variable,
        ':',
        token.immediate(/\s/),
        type,
        optional(seq(
          '=',
          value,
        )),
      ));
    },

    assign: ($, previous) => {
      const lhs = field('lhs', choice(
        $.underscore,
        $.identifier,
        alias('property', $.identifier),
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
        $.assign_call,
        $.index_call,
        alias($.index_operator, $.index_call),
        $.special_variable,
      ));
      const rhs = field('rhs', choice($._expression, $.property_identifier));

      return prec('assignment_operator', seq(
        lhs,
        '=',
        rhs,
      ));
    },

    operator_assign: ($, previous) => {
      const combined_operators = [
        '+=',
        '&+=',
        '-=',
        '&-=',
        '*=',
        '&*=',
        '/=',
        '//=',
        '%=',
        '|=',
        '&=',
        '^=',
        '**=',
        '<<=',
        '>>=',
        '||=',
        '&&=',
      ];

      const lhs = field('lhs', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
        $.assign_call,
        $.index_call,
        alias($.index_operator, $.index_call),
      ));

      const rhs = field('rhs', $._expression);
      const operator = alias(
        choice(...combined_operators),
        $.operator,
      );

      return prec('assignment_operator', seq(lhs, operator, rhs));
    },

    lhs_splat: ($, previous) => seq('*', choice(
      $.underscore,
      $.identifier,
      alias($.semgrep_metavariable, $.identifier),
      $.instance_var,
      $.class_var,
      $.macro_var,
      $.assign_call,
      $.index_call,
      alias($.index_operator, $.index_call),
    )),

    multi_assign: ($, previous) => {
      const lhs_basic = choice(
        $.underscore,
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.macro_var,
        $.assign_call,
        $.index_call,
        alias($.index_operator, $.index_call),
      );
      const lhs_splat = field('lhs', alias($.lhs_splat, $.splat));
      const lhs = field('lhs', choice(lhs_basic, alias($.lhs_splat, $.splat)));
      const multi_lhs = seq(repeat1(seq(lhs, ',')), lhs);

      const rhs = field('rhs', $._expression);
      const multi_rhs = seq(repeat1(seq(rhs, ',')), rhs);

      return choice(
        seq(lhs_splat, '=', rhs),
        seq(multi_lhs, '=', rhs),
        seq(lhs_splat, '=', multi_rhs),
        seq(multi_lhs, '=', multi_rhs),
      );
    },

    uninitialized_assign: ($, previous) => seq(
      field('lhs', choice(
        $.identifier,
        alias($.semgrep_metavariable, $.identifier),
        $.instance_var,
        $.class_var,
        $.global_var,
        $.macro_var,
      )),
      '=',
      field('rhs', $.uninitialized_var),
    ),

    keyword_type_declaration: $ => prec('assignment_operator', seq(
      field('var', alias(
        choice('else', 'then', 'ensure', 'rescue'),
        $.identifier,
      )),
      ':',
      token.immediate(/\s/),
      field('type', $._bare_type),
      optional(seq(
        '=',
        field('value', $._expression),
      )),
    )),

    property_assign_declaration: $ => prec('assignment_operator', seq(
      field('var', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias('else', $.identifier),
        alias('then', $.identifier),
        alias('ensure', $.identifier),
        alias('rescue', $.identifier),
      )),
      '=',
      field('value', $._expression),
    )),

    property_bare_declaration: $ => prec(-1, seq(
      field('var', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias('else', $.identifier),
        alias('then', $.identifier),
        alias('ensure', $.identifier),
        alias('rescue', $.identifier),
      )),
    )),

    property_argument_list: $ => choice(
      $.type_declaration,
      $.keyword_type_declaration,
      $.property_assign_declaration,
      $.property_bare_declaration,
      alias($.unquoted_symbol, $.symbol),
      alias($.quoted_symbol, $.symbol),
      alias($.operator_symbol, $.symbol),
    ),

    _macro_content: ($, previous) => choice(
      previous,
      alias('}}', $.macro_content),
    ),

    named_expr: ($, previous) => {
      const name = field('name', choice(
        $.identifier,
        alias('with', $.identifier),
        alias($._constant_segment, $.identifier),
        alias($.identifier_method_call, $.identifier),
        $.string,
        alias($.string_percent_literal, $.string),
      ));

      const value = choice($._expression, $.out);
      return choice(
        seq(
          name,
          token.immediate(':'),
          value,
        ),
        seq(
          field('name', alias($.keyword_named_argument_name, $.identifier)),
          value,
        ),
      );
    },

    keyword_named_argument_name: $ => token(prec(1000, choice('begin:', 'end:'))),

    argument_list_no_parens: ($, previous) => {
      const args = choice(
        $._expression,
        $.splat,
        $.double_splat,
        $.named_expr,
        $.out,
      );

      return prec.right(seq(
        optional($._start_of_parenless_args),
        args,
        repeat(prec('comma', seq(',', args))),
      ));
    },

    argument_list_no_parens_with_block: ($, previous) => {
      const args = choice(
        $._expression,
        $.splat,
        $.double_splat,
        $.named_expr,
        $.out,
      );

      return prec.right(seq(
        optional($._start_of_parenless_args),
        optional(seq(
          args,
          repeat(prec('comma', seq(',', args))),
          ',',
        )),
        $.block_argument,
      ));
    },

    argument_list_with_parens: ($, previous) => {
      const args = choice(
        $._expression,
        $.property_identifier,
        $.splat,
        $.double_splat,
        $.named_expr,
        $.out,
      );

      return prec.right(seq(
        token.immediate('('),
        optional(seq(
          args,
          repeat(seq(',', args)),
          optional(','),
        )),
        ')',
      ));
    },

    argument_list_with_parens_and_block: ($, previous) => {
      const args = choice(
        $._expression,
        $.property_identifier,
        $.splat,
        $.double_splat,
        $.named_expr,
        $.out,
      );

      return prec.right(seq(
        token.immediate('('),
        optional(seq(
          args,
          repeat(seq(',', args)),
          ',',
        )),
        $.block_argument,
        ')',
      ));
    },

  }
});
