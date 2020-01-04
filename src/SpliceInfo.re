
type splice_name = int;

type splice_map('exp) = GeneralUtil.NatMap.t((HTyp.t, 'exp));

type t('exp) = {
  next: splice_name,
  splice_map: splice_map('exp),
  splice_order: list(splice_name),
};

